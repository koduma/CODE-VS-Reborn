#include <vector>
#include <cfloat>
#include <cstdio>
#include <cstring>
#include <climits>
#include <ctime>
#include <cstdlib>
#include <cmath>
#include <string>
#include <iostream>
#include <cstdint>
#include <algorithm>
#include <cassert>
#include <random>
#include <queue>
#include <deque>
#include <list>
#include <map>
#include <array>
#include <chrono>
#include <fstream>
#include <functional>
#include <unordered_map>
#include <omp.h>

using namespace std;

typedef long long ll;

#define MAX_TURN 500 // ゲームの最大ターン数
#define FIELD_WIDTH 10 // フィールドの横幅
#define FIELD_HEIGHT 16 // フィールドの縦幅
#define WIDTH 12  // 余分な領域も含めたフィールドの横幅
#define HEIGHT 21 // 余分な領域も含めたフィールドの縦幅
#define DANGER_LINE 17 // 危険ライン

#define DELETED_SUM 10 // 消滅のために作るべき和の値
#define EXPLODE 5 //爆発ブロック
#define FIRE_SCORE 45 //発火スコア
#define FIRE_SCORE2 45 //発火スコア2
#define EXPLODE_SCORE 50 //爆発スコア

#define EMPTY 0// 空のグリッド
#define OJAMA 11 // お邪魔ブロック

#define BASE_BEAM_WIDTH 2500// ビーム幅
#define SEARCH_DEPTH 14 // ビームサーチの探索の深さ

#define X(PT) ((PT>>2)&15)
#define ROT(PT) (PT&3)
#define COMMAND(x,rot) ((x)<<2|(rot))

/**
 * 乱数生成器
 */
unsigned long long xor128() {
	static unsigned long long rx = 123456789, ry = 362436069, rz = 521288629, rw = 88675123;
	unsigned long long rt = (rx ^ (rx << 11));
	rx = ry; ry = rz; rz = rw;
	return (rw = (rw ^ (rw >> 19)) ^ (rt ^ (rt >> 8)));
}
struct Pack {
	char t[4];

	Pack() {
		memset(t, -1, sizeof(t));
	}
};
struct Action {
	char command;
	int score;
	int fireTurn;

	Action(char command = -1, int score = 0, int fireTurn = MAX_TURN) {
		this->command = command;
		this->score = score;
		this->fireTurn = fireTurn;
	}
};

struct Node {
	int value;
	int score;
	bool chain;
	char command;
	char field[HEIGHT][WIDTH];

	Node() {
		this->value = 0;
		this->chain = false;
	}

	bool operator >(const Node& n) const {
		return value < n.value;
	}
}fff[BASE_BEAM_WIDTH * 6 * 36];

ll hashCode(char field[HEIGHT][WIDTH], ll g_zoblishField[HEIGHT][WIDTH][12]) {
	ll hash = 0;
	for (int y = 1; y <= FIELD_HEIGHT; ++y) {
		for (int x = 1; x <= FIELD_WIDTH; ++x) {
			int num = field[y][x];
			if (num == EMPTY) { continue; }
			hash ^= g_zoblishField[y][x][num];
		}
	}

	return hash;
}

int skill_chain(char field[HEIGHT][WIDTH]);

Action getBestAction(int turn, char g_field[HEIGHT][WIDTH], Pack g_packs[MAX_TURN], int g_scoreLimit, int BEAM_WIDTH,
	ll g_zoblishField[HEIGHT][WIDTH][12], int threshold);

Action getBestAction2(int turn, char g_field[HEIGHT][WIDTH], Pack g_packs[MAX_TURN], int g_scoreLimit, int BEAM_WIDTH,
	ll g_zoblishField[HEIGHT][WIDTH][12]);

/**
 * @param [int] x x座標の値
 */
inline void setPutPackLine(int x, char g_field[HEIGHT][WIDTH], char g_putPackLine[WIDTH]) {
	int y = 1;

	while (g_field[y][x] != EMPTY && y < HEIGHT - 1) {
		++y;
	}

	g_putPackLine[x] = y;
}
/**
 * ブロックを落下させる時に落下させる位置を更新する
 */
inline void updatePutPackLine(char g_field[HEIGHT][WIDTH], char g_putPackLine[WIDTH]) {
	for (int x = 1; x <= FIELD_WIDTH; ++x) {
		setPutPackLine(x, g_field, g_putPackLine);
	}
}
/**
 * パック情報を読み込む
 * お邪魔が全ての埋まった状態のパックも用意しておく
 */
void readPackInfo(Pack g_packs[MAX_TURN], Pack g_ojamaPacks[MAX_TURN]) {
	fprintf(stderr, "readPackInfo =>\n");
	int t0, t1, t2, t3;
	string _end_;

	for (int i = 0; i < MAX_TURN; i++) {
		Pack pack;
		cin >> t0 >> t1;
		cin >> t2 >> t3;

		pack.t[0] = t0; pack.t[1] = t1;
		pack.t[2] = t2; pack.t[3] = t3;

		g_packs[i] = pack;
		g_ojamaPacks[i] = pack;

		for (int j = 0; j < 4; j++) {
			if (g_ojamaPacks[i].t[j] == EMPTY) {
				g_ojamaPacks[i].t[j] = OJAMA;
			}
		}

		cin >> _end_;
	}
}
/**
 * 1. ゲーム開始時の入力情報を読み込む
 * 2. フィールド情報を初期化しておく
 * 3. zoblish hash用の乱数テーブル生成
 * 4. パック情報の読み込み
 */
void init(char g_myField[HEIGHT][WIDTH], char g_enemyField[HEIGHT][WIDTH], char g_field[HEIGHT][WIDTH]
	, ll g_zoblishField[HEIGHT][WIDTH][12], Pack g_packs[MAX_TURN], Pack g_ojamaPacks[MAX_TURN], int* beforeTime) {

	char tmp1[HEIGHT][WIDTH];

	memset(g_myField, EMPTY, sizeof(tmp1));
	memset(g_enemyField, EMPTY, sizeof(tmp1));
	memset(g_field, EMPTY, sizeof(tmp1));

	for (int y = 0; y < HEIGHT; ++y) {
		for (int x = 0; x < WIDTH; ++x) {
			for (int i = 0; i < 12; i++) {
				g_zoblishField[y][x][i] = xor128();
			}
		}
	}
	*beforeTime = 180000;

	readPackInfo(g_packs, g_ojamaPacks);
}
/**
 * ターン毎の情報を読み込む
 */
int readTurnInfo(int* beforeTime, int* myRemainTime, int* myOjamaStock, char g_myField[HEIGHT][WIDTH]
	, char g_enemyField[HEIGHT][WIDTH], int* g_scoreLimit, int* skillgauge, int* flag) {

	string _end_;
	int ret;

	// [現在のターン数]
	int turn;
	cin >> turn;

	// [自分の残り思考時間。単位はミリ秒]
	cin >> *myRemainTime;

	fprintf(stderr, "%2d:MRT=%d,UT=%d\n", turn, *myRemainTime, *beforeTime - *myRemainTime);

	ret = (*beforeTime) - (*myRemainTime);

	*beforeTime = *myRemainTime;

	// [自分のお邪魔ストック]
	cin >> *myOjamaStock;

	//　[自分のスキルゲージ]
	cin >> *skillgauge;

	//　[自分のスコア]
	int myscore;
	cin >> myscore;

	int ojamaCnt = 0;
	int block = 0;

	// [前のターン終了時の自分のフィールド情報]
	int t;
	for (int y = 0; y < FIELD_HEIGHT; ++y) {
		for (int x = 1; x <= FIELD_WIDTH; ++x) {
			cin >> t;
			g_myField[FIELD_HEIGHT - y][x] = t;
			if (t == OJAMA) {
				ojamaCnt++;
			}
			if (t != EMPTY) { block++; }
		}
	}

	cin >> _end_;

	//  [相手の残り思考時間。単位はミリ秒]
	int enemyRemainTime;
	cin >> enemyRemainTime;

	// [相手のお邪魔ストック]
	int enemyOjamaStock;
	cin >> enemyOjamaStock;

	int enemy_skillgauge;
	cin >> enemy_skillgauge;

	int enemy_score;
	cin >> enemy_score;

	// [前のターン終了時の相手のフィールド情報]
	for (int y = 0; y < FIELD_HEIGHT; ++y) {
		for (int x = 1; x <= FIELD_WIDTH; ++x) {
			cin >> t;
			g_enemyField[FIELD_HEIGHT - y][x] = t;
		}
	}

	if (enemy_skillgauge >= 70 && *flag == 0) {
		*flag = 1;
	}

	if (block >= 70) {
		*g_scoreLimit = 2 * EXPLODE_SCORE;//不利
	}
	else {
		if (*flag == 1) { *g_scoreLimit = 2 * FIRE_SCORE2; }//ボマー対策
		else {
			*g_scoreLimit = 2 * FIRE_SCORE;//普通
		}
	}

	cin >> _end_;
	return ret;
}
/**
 * パックにお邪魔を埋め込む
 *
 * @param [int] turn 現在のターン
 * @param [int] ojamaStock 現在のお邪魔のストック数
 */
void fillOjama(int turn, int ojamaStock, Pack g_packs[MAX_TURN]) {
	for (int t = turn; t < MAX_TURN && ojamaStock > 0; t++) {
		Pack* pack = &g_packs[t];

		for (int i = 0; i < 4 && ojamaStock > 0; i++) {
			if (pack->t[i] == EMPTY) {
				pack->t[i] = OJAMA;
				ojamaStock--;
			}
		}
	}
}
/**
 * ブロックからお邪魔を取り除く
 *
 * @param [int] turn 現在のターン
 * @param [int] ojamaStock 現在のお邪魔のストック数
 */
void cleanOjama(int turn, int ojamaStock, Pack g_packs[MAX_TURN]) {
	for (int t = turn; t < MAX_TURN && ojamaStock > 0; t++) {
		Pack* pack = &g_packs[t];

		for (int i = 0; i < 4 && ojamaStock > 0; i++) {
			if (pack->t[i] == OJAMA) {
				pack->t[i] = EMPTY;
				ojamaStock--;
			}
		}
	}
}
/**
 * 自分のベストなコマンドを選択する
 *
 * @return [Action] 一番良いアクション
 */
Action getMyBestAction(int turn, char g_field[HEIGHT][WIDTH], char g_myField[HEIGHT][WIDTH], int myRemainTime
	, Pack g_packs[MAX_TURN], int g_scoreLimit, ll g_zoblishField[HEIGHT][WIDTH][12], int threshold) {

	int BEAM_WIDTH;

	char tmp[HEIGHT][WIDTH];

	memcpy(g_field, g_myField, sizeof(tmp));

	if (myRemainTime >= 60000) {
		BEAM_WIDTH = 6 * BASE_BEAM_WIDTH;
	}
	else if (myRemainTime < 30000) {
		BEAM_WIDTH = BASE_BEAM_WIDTH / 2;
	}
	else {
		BEAM_WIDTH = BASE_BEAM_WIDTH;
	}

	if (threshold == -1) {
		return getBestAction2(turn, g_field, g_packs, g_scoreLimit, BEAM_WIDTH, g_zoblishField);
	}
	else {
		return getBestAction(turn, g_field, g_packs, g_scoreLimit, BEAM_WIDTH, g_zoblishField, threshold);
	}
}
/**
 * AIのメインの処理部分
 *
 * @param [int] turn 現在のターン数
 */
void run(int turn, int* beforeTime, int* myRemainTime, int* myOjamaStock, int* g_scoreLimit, char g_myField[HEIGHT][WIDTH]
	, char g_enemyField[HEIGHT][WIDTH], Pack g_packs[MAX_TURN], char g_field[HEIGHT][WIDTH],
	ll g_zoblishField[HEIGHT][WIDTH][12], int* skillgauge, int* flag) {

	int ut = readTurnInfo(beforeTime, myRemainTime, myOjamaStock, g_myField, g_enemyField, g_scoreLimit, skillgauge, flag);

	if (ut >= 20000) {
		cout << 15 << " " << 3 << endl;
		fflush(stderr);
		return;
	}

	char tmp[HEIGHT][WIDTH];
	char tmp2[WIDTH];
	updatePutPackLine(g_myField, tmp2);

	if (*myOjamaStock > 0) {
		//fillOjama(turn, *myOjamaStock, g_packs);
	}
	if (*myOjamaStock >= 10) {
		for (int x = 1; x <= FIELD_WIDTH; x++) {
			g_myField[tmp2[x]][x] = OJAMA;
		}
	}

	memcpy(tmp, g_myField, sizeof(tmp));

	Action action;

	if (*g_scoreLimit != 2 * EXPLODE_SCORE) {
		action = getMyBestAction(turn, g_field, g_myField, *myRemainTime, g_packs, *g_scoreLimit, g_zoblishField, -1);
	}
	else {

		int threshold = 0;

		if (*skillgauge < 80) { threshold = 1; }

		action = getMyBestAction(turn, g_field, g_myField, *myRemainTime, g_packs, *g_scoreLimit, g_zoblishField, threshold);
	}

	char command = action.command;
	int MNP = action.score;

	fprintf(stderr, "%2d: MNP=%d, FT=%d\n", turn, MNP, action.fireTurn);

	if (MNP >= 2 * FIRE_SCORE2 && turn == action.fireTurn) {
		*flag = -1;
	}

	int use_skill_score = skill_chain(tmp);

	if (*skillgauge >= 80 && use_skill_score >= *g_scoreLimit) {
		cout << "S" << endl;
		fflush(stderr);
	}
	else {
		cout << X(command) << " " << ROT(command) << endl;
		fflush(stderr);
	}

	if (*myOjamaStock > 0) {
		//cleanOjama(turn, *myOjamaStock, g_packs);
	}
}
/**
 * 連鎖判定が必要な場所にチェックを行う
 *
 * @param [int] y ブロックが落下したy座標
 * @param [int] x ブロックが落下したx座標
 */
void setChainCheckId(int y, int x, char g_chainCheck[HEIGHT][WIDTH]) {
	g_chainCheck[y][x] = 1;
}
/**
 * 指定したx座標にパックの一部を落とす (下のような感じで落とす)
 *
 *    t0 t1 t2
 *
 * @param [int] x x座標
 * @param [int] t0 パックのブロックの値
 * @param [int] t1 パックのブロックの値
 * @param [int] t2 パックのブロックの値
 * @param [bool] 設置可能かどうか
 */
bool putLinePack(int x, int t0, int t1, char g_putPackLine[WIDTH], char g_field[HEIGHT][WIDTH],
	char g_chainCheck[HEIGHT][WIDTH]) {

	int y = g_putPackLine[x];

	assert(t0 >= 0);
	assert(t1 >= 0);

	if (t0 != EMPTY) {
		if (x < 1 || x > FIELD_WIDTH) { return false; }
		g_field[y][x] = t0;
		setChainCheckId(y, x, g_chainCheck);
		++y;
	}
	if (t1 != EMPTY) {
		if (x < 1 || x > FIELD_WIDTH) { return false; }
		g_field[y][x] = t1;
		setChainCheckId(y, x, g_chainCheck);
		++y;
	}

	assert(y <= HEIGHT);
	g_putPackLine[x] = y;
	return true;
}
/**
 * パックを設置する処理
 *
 * @param [int] x 設置するx座標
 * @param [int] rot 回転数
 * @param [Pack] pack パック情報
 * @return [bool] 設置が成功したかどうか
 */
bool putPack(int x, int rot, const Pack& pack, char g_putPackLine[WIDTH], char g_field[HEIGHT][WIDTH]
	, char g_chainCheck[HEIGHT][WIDTH]) {
	bool success = true;

	switch (rot) {
	case 0:
		success &= putLinePack(x + 1, pack.t[2], pack.t[0], g_putPackLine, g_field, g_chainCheck);
		success &= putLinePack(x + 2, pack.t[3], pack.t[1], g_putPackLine, g_field, g_chainCheck);
		break;
	case 1:
		success &= putLinePack(x + 1, pack.t[3], pack.t[2], g_putPackLine, g_field, g_chainCheck);
		success &= putLinePack(x + 2, pack.t[1], pack.t[0], g_putPackLine, g_field, g_chainCheck);
		break;
	case 2:
		success &= putLinePack(x + 1, pack.t[1], pack.t[3], g_putPackLine, g_field, g_chainCheck);
		success &= putLinePack(x + 2, pack.t[0], pack.t[2], g_putPackLine, g_field, g_chainCheck);
		break;
	case 3:
		success &= putLinePack(x + 1, pack.t[0], pack.t[1], g_putPackLine, g_field, g_chainCheck);
		success &= putLinePack(x + 2, pack.t[2], pack.t[3], g_putPackLine, g_field, g_chainCheck);
		break;
	default:
		assert(false);
	}

	return success;
}
/**
 * フィールドの最大の高さを更新する
 */
inline void updateMaxHeight(int* g_maxHeight, char g_putPackLine[WIDTH]) {
	*g_maxHeight = 0;

	for (int x = 1; x <= FIELD_WIDTH; ++x) {
		*g_maxHeight = max(*g_maxHeight, g_putPackLine[x] - 1);
	}
}
void deleteCheck(int y, int x, char g_field[HEIGHT][WIDTH], char g_packDeleteChecker[HEIGHT][WIDTH], int* g_deleteCount) {

	int tmp = *g_deleteCount;

	int dy[8] = { -1,-1,-1,0,0,1,1,1 };
	int dx[8] = { -1,0,1,-1,1,-1,0,1 };

	for (int k = 0; k < 8; k++) {
		if (1 <= y + dy[k] && y + dy[k] <= FIELD_HEIGHT && 1 <= x + dx[k] && x + dx[k] <= FIELD_WIDTH) {
			if (g_field[y][x] + g_field[y + dy[k]][x + dx[k]] == DELETED_SUM) {
				if (g_packDeleteChecker[y + dy[k]][x + dx[k]] != 1) {
					g_packDeleteChecker[y + dy[k]][x + dx[k]] = 1;
					tmp++;
				}
				if (g_packDeleteChecker[y][x] != 1) {
					g_packDeleteChecker[y][x] = 1;
					tmp++;
				}
			}
		}
	}
	*g_deleteCount = tmp;
}
/**
 * 連鎖判定を行う
 */
void chainPack(int* g_deleteCount, int g_maxHeight, char g_field[HEIGHT][WIDTH], char g_packDeleteChecker[HEIGHT][WIDTH]
	, char g_chainCheck[HEIGHT][WIDTH]) {

	*g_deleteCount = 0;
	for (int y = 1; y <= g_maxHeight; y++) {
		for (int x = 1; x <= FIELD_WIDTH; x++) {
			if (g_chainCheck[y][x] == 1) {
				deleteCheck(y, x, g_field, g_packDeleteChecker, g_deleteCount);
				g_chainCheck[y][x] = 0;
			}
		}
	}
}
/**
 * パックの落下処理と削除処理を行う
 */
void fallPack(char g_field[HEIGHT][WIDTH], char g_putPackLine[WIDTH], char g_packDeleteChecker[HEIGHT][WIDTH],
	char g_chainCheck[HEIGHT][WIDTH]) {

	for (int x = 1; x <= FIELD_WIDTH; ++x) {
		int fallCnt = 0;
		int limitY = g_putPackLine[x];

		for (int y = 1; y < limitY; ++y) {
			if (g_packDeleteChecker[y][x] == 1) {
				g_field[y][x] = EMPTY;
				fallCnt++;
				g_putPackLine[x]--;
			}
			else if (fallCnt > 0) {
				int t = y - fallCnt;
				g_field[t][x] = g_field[y][x];
				g_field[y][x] = EMPTY;
				setChainCheckId(t, x, g_chainCheck);
			}
		}
	}
}

/**
 * 連鎖処理のシミュレーションを行う
 *
 * @return [int] スコア
 */
int simulate(int* g_maxHeight, char g_putPackLine[WIDTH], char g_field[HEIGHT][WIDTH], char g_packDeleteChecker[HEIGHT][WIDTH],
	char g_chainCheck[HEIGHT][WIDTH]) {

	int chainCnt = 0;
	int score = 0;
	int g_deleteCount = 0;

	char tmp[HEIGHT][WIDTH];

	while (1) {

		updateMaxHeight(g_maxHeight, g_putPackLine);

		chainPack(&g_deleteCount, *g_maxHeight, g_field, g_packDeleteChecker, g_chainCheck);

		fallPack(g_field, g_putPackLine, g_packDeleteChecker, g_chainCheck);

		memset(g_packDeleteChecker, 0, sizeof(tmp));

		if (g_deleteCount == 0) { break; }

		chainCnt++;
		score += (int)floor(pow(1.3, chainCnt));

	}

	return score;
}

int evaluate(int* g_maxHeight, char g_field[HEIGHT][WIDTH], char g_putPackLine[WIDTH],
	char g_packDeleteChecker[HEIGHT][WIDTH], char g_chainCheck[HEIGHT][WIDTH]) {

	int maxValue = 0;
	char g_tempField[HEIGHT][WIDTH];
	char g_tempPutPackLine[WIDTH];
	memcpy(g_tempField, g_field, sizeof(g_tempField));
	memcpy(g_tempPutPackLine, g_putPackLine, sizeof(g_tempPutPackLine));

	int dy[9] = { -1,-1,-1,0,0,0,1,1,1 };
	int dx[9] = { -1,0,1,-1,0,1,-1,0,1 };

	memset(g_packDeleteChecker, 0, sizeof(g_tempField));
	memset(g_chainCheck, 0, sizeof(g_tempField));


	int score = 0;

	double explode = 0;

	for (int x = 1; x <= FIELD_WIDTH; x++) {
		int limitY = g_putPackLine[x];
		for (int y = 1; y < limitY; y++) {
			if (g_field[y][x] == EXPLODE) {
				for (int k = 0; k < 9; k++) {
					if (1 <= y + dy[k] && y + dy[k] <= FIELD_HEIGHT && 1 <= x + dx[k] && x + dx[k] <= FIELD_WIDTH) {
						if (EMPTY < g_field[y + dy[k]][x + dx[k]] && g_field[y + dy[k]][x + dx[k]] < OJAMA &&
							g_packDeleteChecker[y + dy[k]][x + dx[k]] == 0) {
							g_packDeleteChecker[y + dy[k]][x + dx[k]] = 1;
							explode += 1.0;
						}
					}
				}
			}
		}
	}

	if (explode < 0.1) { return 0; }

	score += (int)floor(25.0 * pow(2.0, explode / 12.0));


	fallPack(g_field, g_putPackLine, g_packDeleteChecker, g_chainCheck);

	score += simulate(g_maxHeight, g_putPackLine, g_field, g_packDeleteChecker, g_chainCheck);

	memcpy(g_field, g_tempField, sizeof(g_tempField));
	memcpy(g_putPackLine, g_tempPutPackLine, sizeof(g_tempPutPackLine));

	return score;
}
int evaluate2(int* g_maxHeight, char g_field[HEIGHT][WIDTH], char g_putPackLine[WIDTH],
	char g_packDeleteChecker[HEIGHT][WIDTH], char g_chainCheck[HEIGHT][WIDTH]) {

	int maxValue = 0;
	char g_tempField[HEIGHT][WIDTH];
	char g_tempPutPackLine[WIDTH];
	memcpy(g_tempField, g_field, sizeof(g_tempField));
	memcpy(g_tempPutPackLine, g_putPackLine, sizeof(g_tempPutPackLine));

	int dy[8] = { -1,-1,-1,0,0,1,1,1 };
	int dx[8] = { -1,0,1,-1,1,-1,0,1 };

	for (int x = 1; x <= FIELD_WIDTH; x++) {

		//int t = g_putPackLine[x];
		//int limit = min(13, t + 4);

		//for (int y = t; y <= limit; y++) {

		for (int num = 1; num <= 9; num++) {

			memset(g_packDeleteChecker, 0, sizeof(g_tempField));
			memset(g_chainCheck, 0, sizeof(g_tempField));

			int y = g_putPackLine[x];

			int ok = 0;

			for (int i = 0; i < 8; i++) {
				if (1 <= y + dy[i] && y + dy[i] <= FIELD_HEIGHT && 1 <= x + dx[i] && x + dx[i] <= FIELD_WIDTH &&
					num + g_field[y + dy[i]][x + dx[i]] == DELETED_SUM) {
					ok = 1;
					break;
				}
			}

			if (ok == 1) {

				g_field[y][x] = num;

				setChainCheckId(y, x, g_chainCheck);

				g_putPackLine[x] = y + 1;

				int score = simulate(g_maxHeight, g_putPackLine, g_field, g_packDeleteChecker, g_chainCheck);

				maxValue = max(maxValue, score);

				memcpy(g_field, g_tempField, sizeof(g_tempField));
				memcpy(g_putPackLine, g_tempPutPackLine, sizeof(g_tempPutPackLine));
			}

		}
		//}
	}

	/*

	double avg[10][2] = { 0 };
	double cnt[10] = { 0 };

	for (int y = 1; y <= FIELD_HEIGHT; y++) {
		for (int x = 1; x <= FIELD_WIDTH; x++) {
			if (EMPTY < g_field[y][x] && g_field[y][x] < OJAMA) {
				avg[g_field[y][x]][0] += (double)y;
				avg[g_field[y][x]][1] += (double)x;
				cnt[g_field[y][x]] += 1.0;
			}
		}
	}



	double add = 0;

	for (int i = 1; i <= 4; i++) {
		if (cnt[i] > 0) {
			avg[i][0] /= cnt[i];
			avg[i][1] /= cnt[i];
		}
		if (cnt[10 - i] > 0) {
			avg[10 - i][0] /= cnt[10 - i];
			avg[10 - i][1] /= cnt[10 - i];
		}
	}

	for (int i = 1; i <= 4; i++) {
		add += max(avg[i][0] - avg[10 - i][0], avg[10 - i][0] - avg[i][0]);
		add += max(avg[i][1] - avg[10 - i][1], avg[10 - i][1] - avg[i][1]);
	}

	add *= 0.5;

	maxValue -= (int)add;

	*/

	return maxValue;
}
void fall(char field[HEIGHT][WIDTH]) {

	int i, j;

	for (j = 1; j <= FIELD_WIDTH; j++) {
		int tgt = 1;
		for (i = 1; i <= FIELD_HEIGHT; i++) {
			if (field[i][j] != 0) {
				char c = field[i][j];
				field[i][j] = 0;
				field[tgt][j] = c;
				tgt++;
			}
		}
	}
}
int skill_chain(char field[HEIGHT][WIDTH]) {

	int dy[8] = { -1,-1,-1,0,0,1,1,1 };
	int dx[8] = { -1,0,1,-1,1,-1,0,1 };

	int dy2[9] = { -1,-1,-1,0,0,0,1,1,1 };
	int dx2[9] = { -1,0,1,-1,0,1,-1,0,1 };

	int chainCnt = 0;
	int deletecount = 0;
	int score = 0;
	int skill_chain_score = 0;
	int explode_score = 0;

	char deletee[HEIGHT][WIDTH];

	double explode = 0;

	for (int y = 1; y <= FIELD_HEIGHT; y++) {
		for (int x = 1; x <= FIELD_WIDTH; x++) {
			if (field[y][x] == EXPLODE) {
				for (int k = 0; k < 9; k++) {
					if (1 <= y + dy2[k] && y + dy2[k] <= FIELD_HEIGHT && 1 <= x + dx2[k] && x + dx2[k] <= FIELD_WIDTH) {
						if (EMPTY < field[y + dy2[k]][x + dx2[k]] && field[y + dy2[k]][x + dx2[k]] < OJAMA) {
							field[y + dy2[k]][x + dx2[k]] = EMPTY;
							explode += 1.0;
						}
					}
				}
			}
		}
	}

	if (explode < 0.1) { return 0; }

	explode_score = (int)floor(25.0 * pow(2.0, explode / 12.0));

	score += (int)floor(explode_score);

	fall(field);

	while (1) {

		memset(deletee, 0, sizeof(deletee));

		deletecount = 0;

		for (int y = 1; y <= FIELD_HEIGHT; y++) {
			for (int x = 1; x <= FIELD_WIDTH; x++) {
				for (int k = 0; k < 8; k++) {
					if (1 <= y + dy[k] && y + dy[k] <= FIELD_HEIGHT && 1 <= x + dx[k] && x + dx[k] <= FIELD_WIDTH) {
						if (field[y][x] + field[y + dy[k]][x + dx[k]] == DELETED_SUM) {
							deletee[y][x] = 1;
							deletee[y + dy[k]][x + dx[k]] = 1;
						}
					}
				}
			}
		}
		for (int y = 1; y <= FIELD_HEIGHT; y++) {
			for (int x = 1; x <= FIELD_WIDTH; x++) {
				if (deletee[y][x] == 1) {
					field[y][x] = EMPTY;
					deletecount++;
				}
			}
		}

		fall(field);

		if (deletecount == 0) { break; }

		chainCnt++;
		skill_chain_score += (int)floor(pow(1.3, chainCnt));


	}//while

	score += (int)floor(skill_chain_score);

	return score;

}

/**
 * 一番良い操作を取得する
 *
 * @param [int] turn 今現在のターン
 * @return [Action] 一番ベストな行動情報
 */
Action getBestAction(int turn, char g_field[HEIGHT][WIDTH], Pack g_packs[MAX_TURN], int g_scoreLimit, int BEAM_WIDTH,
	ll g_zoblishField[HEIGHT][WIDTH][12], int threshold) {

	char tmp1[HEIGHT][WIDTH];

	Node root;
	memcpy(root.field, g_field, sizeof(tmp1));
	Action bestAction;
	int maxValue = -9999;

	deque<Node> que;
	que.push_back(root);

	unordered_map<ll, bool> checkNodeList;

	for (int depth = 0; depth < SEARCH_DEPTH; depth++) {
		priority_queue<Node, vector<Node>, greater<Node> > pque;
		int update = 0;
		int ks = (int)que.size();

#pragma omp parallel for reduction(+:update)
		for (int k = 0; k < ks; k++) {

			Pack pack = g_packs[turn + depth];

			Node node = que[k];

			char g_tempfield[HEIGHT][WIDTH];
			char g_putPackLine[WIDTH];
			char g_tempPutPackLine[WIDTH];
			char g_packDeleteChecker[HEIGHT][WIDTH];
			char g_chainCheck[HEIGHT][WIDTH];

			memcpy(g_tempfield, node.field, sizeof(node.field));
			updatePutPackLine(g_tempfield, g_putPackLine);
			memcpy(g_tempPutPackLine, g_putPackLine, sizeof(g_putPackLine));


			for (int x = 0; x <= 8; x++) {
				for (int rot = 0; rot < 4; rot++) {

					memset(g_packDeleteChecker, 0, sizeof(g_packDeleteChecker));
					memset(g_chainCheck, 0, sizeof(g_chainCheck));

					if (putPack(x, rot, pack, g_putPackLine, g_tempfield,
						g_chainCheck)) {
						Node cand;
						int g_maxHeight;
						cand.score = simulate(&g_maxHeight, g_putPackLine, g_tempfield, g_packDeleteChecker, g_chainCheck);
						if (g_maxHeight < DANGER_LINE) {
							cand.value = cand.score;
							memcpy(cand.field, g_tempfield, sizeof(g_tempfield));
							cand.chain = (cand.score != threshold);
							if (!cand.chain) {
								cand.value = evaluate(&g_maxHeight, g_tempfield, g_putPackLine, g_packDeleteChecker,
									g_chainCheck);
								update++;
							}
							//if (cand.value >= g_scoreLimit) { cand.value += 100 * cand.score; }
							cand.command = (depth == 0) ? COMMAND(x, rot) : node.command;
							//#pragma omp critical
														//{ pque.push(cand); }
														//pque.push(cand);
							fff[(36 * k) + (4 * x) + rot] = cand;
						}
						else {
							cand.value = -114514;
							cand.score = -114514;
							fff[(36 * k) + (4 * x) + rot] = cand;
						}
					}
					else {
						Node cand2;
						cand2.value = -114514;
						cand2.score = -114514;
						fff[(36 * k) + (4 * x) + rot] = cand2;
					}
					memcpy(g_tempfield, node.field, sizeof(node.field));
					memcpy(g_putPackLine, g_tempPutPackLine, sizeof(g_tempPutPackLine));
				}
			}
		}
		que.clear();
		vector<pair<int, int> >vec;
		for (int j = 0; j < 36 * ks; j++) {
			vec.push_back(make_pair(fff[j].value, j));
		}
		sort(vec.begin(), vec.end(), greater<>());
		for (int j = 0; j < BEAM_WIDTH && j < 36 * ks; j++) {
			Node node = fff[vec[j].second];

			if (node.score == -114514) { continue; }

			if (node.value >= g_scoreLimit) {
				return Action(node.command, node.value, turn + depth);
			}

			if (maxValue < node.value) {
				maxValue = node.value;
				bestAction = Action(node.command, node.value, turn + depth);
			}

			if (depth < SEARCH_DEPTH - 1) {
				ll hash = hashCode(node.field, g_zoblishField);

				if (!checkNodeList[hash] && (!node.chain || (update == 0 && node.score <= 2))) {
					checkNodeList[hash] = true;
					que.push_back(node);
				}
			}
		}
	}

	return bestAction;
}

Action getBestAction2(int turn, char g_field[HEIGHT][WIDTH], Pack g_packs[MAX_TURN], int g_scoreLimit, int BEAM_WIDTH,
	ll g_zoblishField[HEIGHT][WIDTH][12]) {

	char tmp1[HEIGHT][WIDTH];

	Node root;
	memcpy(root.field, g_field, sizeof(tmp1));
	Action bestAction;
	int maxValue = -9999;

	deque<Node> que;
	que.push_back(root);

	unordered_map<ll, bool> checkNodeList;

	for (int depth = 0; depth < SEARCH_DEPTH; depth++) {
		priority_queue<Node, vector<Node>, greater<Node> > pque;
		int update = 0;
		int ks = (int)que.size();
#pragma omp parallel for reduction(+:update)
		for (int k = 0; k < ks; k++) {

			Pack pack = g_packs[turn + depth];


			Node node = que[k];

			char g_tempfield[HEIGHT][WIDTH];
			char g_putPackLine[WIDTH];
			char g_tempPutPackLine[WIDTH];
			char g_packDeleteChecker[HEIGHT][WIDTH];
			char g_chainCheck[HEIGHT][WIDTH];

			memcpy(g_tempfield, node.field, sizeof(node.field));
			updatePutPackLine(g_tempfield, g_putPackLine);
			memcpy(g_tempPutPackLine, g_putPackLine, sizeof(g_putPackLine));

			for (int x = 0; x <= 8; x++) {
				for (int rot = 0; rot < 4; rot++) {

					memset(g_packDeleteChecker, 0, sizeof(g_packDeleteChecker));
					memset(g_chainCheck, 0, sizeof(g_chainCheck));

					if (putPack(x, rot, pack, g_putPackLine, g_tempfield,
						g_chainCheck)) {
						Node cand;
						int g_maxHeight;
						cand.score = simulate(&g_maxHeight, g_putPackLine, g_tempfield, g_packDeleteChecker, g_chainCheck);
						if (g_maxHeight < DANGER_LINE) {
							cand.value = cand.score;
							memcpy(cand.field, g_tempfield, sizeof(g_tempfield));
							cand.chain = (cand.score > 1 || (depth > 0 && cand.score > 0));
							if (!cand.chain) {
								int ev = evaluate2(&g_maxHeight, g_tempfield, g_putPackLine, g_packDeleteChecker,
									g_chainCheck);
								cand.value += ev;
								update++;
							}
							if (cand.score >= g_scoreLimit) { cand.value += 100 * cand.score; }
							cand.command = (depth == 0) ? COMMAND(x, rot) : node.command;
							//#pragma omp critical
							//							{ pque.push(cand); }
														//pque.push(cand);
							fff[(36 * k) + (4 * x) + rot] = cand;
						}
						else {
							cand.value = -114514;
							cand.score = -114514;
							fff[(36 * k) + (4 * x) + rot] = cand;
						}
					}
					else {
						Node cand2;
						cand2.value = -114514;
						cand2.score = -114514;
						fff[(36 * k) + (4 * x) + rot] = cand2;
					}
					memcpy(g_tempfield, node.field, sizeof(node.field));
					memcpy(g_putPackLine, g_tempPutPackLine, sizeof(g_tempPutPackLine));
				}
			}
		}
		que.clear();
		vector<pair<int, int> >vec;
		for (int j = 0; j < 36 * ks; j++) {
			vec.push_back(make_pair(fff[j].value, j));
		}
		sort(vec.begin(), vec.end(), greater<>());
		for (int j = 0; j < BEAM_WIDTH && j < 36 * ks; j++) {
			Node node = fff[vec[j].second];

			if (node.score == -114514) { continue; }

			if (node.score >= g_scoreLimit) {
				return Action(node.command, node.score, turn + depth);
			}

			if (maxValue < node.score) {
				maxValue = node.score;
				bestAction = Action(node.command, node.score, turn + depth);
			}

			if (depth < SEARCH_DEPTH - 1) {
				ll hash = hashCode(node.field, g_zoblishField);

				if (!checkNodeList[hash] && (!node.chain || (update == 0 && node.score <= 2))) {
					checkNodeList[hash] = true;
					que.push_back(node);
				}
			}
		}
	}

	return bestAction;
}

int main() {

	cout << "tekitouk" << endl;

	int beforeTime;
	int myRemainTime;
	int myOjamaStock;
	int g_scoreLimit;
	int skillgauge;
	int flag = 0;

	ll g_zoblishField[HEIGHT][WIDTH][12];

	char g_myField[HEIGHT][WIDTH] = { 0 };
	char g_enemyField[HEIGHT][WIDTH] = { 0 };
	char g_field[HEIGHT][WIDTH] = { 0 };

	Pack g_packs[MAX_TURN];
	Pack g_ojamaPacks[MAX_TURN];

	init(g_myField, g_enemyField, g_field, g_zoblishField
		, g_packs, g_ojamaPacks, &beforeTime);

	for (int i = 0; i < MAX_TURN; i++) {
		memset(g_myField, 0, sizeof(g_myField));
		memset(g_enemyField, 0, sizeof(g_enemyField));
		memset(g_field, 0, sizeof(g_field));
		run(i, &beforeTime, &myRemainTime, &myOjamaStock, &g_scoreLimit, g_myField, g_enemyField, g_packs, g_field, g_zoblishField, &skillgauge, &flag);
	}

	return 0;
}
