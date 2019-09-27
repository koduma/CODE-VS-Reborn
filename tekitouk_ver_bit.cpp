#define _USE_MATH_DEFINES
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

#include <stack>
#include <iomanip>
#include <tuple>
#include <set>
#include <sstream>
#include <unordered_set>
#ifdef _MSC_VER
#include <intrin.h>
#endif

using namespace std;

typedef unsigned long long ull;
typedef unsigned long u64;

#define MAX_TURN 500 // ゲームの最大ターン数
#define WIDTH 10  //フィールドの横幅
#define HEIGHT 16 // フィールドの縦幅
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

const int BOTTOM = HEIGHT - 1;
const u64 mask4 = 0b1111;//値は15

#ifdef _MSC_VER
inline unsigned long __builtin_clzll(ull x) { unsigned long r; _BitScanReverse64(&r, x); return 63 - r; }
#endif // _MSC_VER

inline unsigned int bsr(ull v) { return 63 - __builtin_clzll(v); } // 最上位の1は下から数えて何ビット目か？

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
	ull field[WIDTH];

	Node() {
		this->value = 0;
		this->chain = false;
	}

	bool operator >(const Node& n) const {
		return value < n.value;
	}
}fff[BASE_BEAM_WIDTH * 6 * 36];


bool IsIn(int x, int y)//x,yが盤面内ならtrue
{
	return 0 <= x && x < WIDTH && 0 <= y && y < HEIGHT;
}

/**
 * 乱数生成器
 */
int xorshift32_s = 7;
unsigned int xorshift32()
{
	xorshift32_s ^= (xorshift32_s << 13);
	xorshift32_s ^= (xorshift32_s >> 17);
	xorshift32_s ^= (xorshift32_s << 15);
	return xorshift32_s;
}

//nの左からshift数分のbitを右にくっつける
ull rotl(ull n, int shift)
{
	return (n << shift) | (n >> (64 - shift));
}
//盤面のハッシュ値を取得
ull GetHash(ull field[WIDTH])
{
	ull a = rotl(field[1], 4);

	ull hash = 0;
	hash ^= field[0] * 1;
	hash ^= rotl(field[1], 30) * 11ull;
	hash ^= rotl(field[2], 5) * 13ull;
	hash ^= rotl(field[3], 35) * 17ull;
	hash ^= rotl(field[4], 10) * 19ull;
	hash ^= rotl(field[5], 40) * 23ull;
	hash ^= rotl(field[6], 15) * 29ull;
	hash ^= rotl(field[7], 45) * 31ull;
	hash ^= rotl(field[8], 20) * 37ull;
	hash ^= rotl(field[9], 50) * 39ull;
	return hash;
}
//x,y座標のブロックの数字を取得
int Get(int x, int y, ull field[WIDTH])
{
	return ((field[x] >> (y * 4)) & mask4);
}

//x,y座標のブロックをブロックnに変える
void SetBit(int x, int y, int n, ull field[WIDTH])
{
	int shift = (y * 4);
	field[x] &= (~((ull)mask4 << shift));
	field[x] |= ((ull)n << shift);
}

//ブロックを置く
bool DropLine(int x, int block_line, ull* check, ull field[WIDTH])
{
	if (block_line == 0) {return 0;}

	//checkは俺のaiでいうg_putPackLineに相当する。
	*check &= ~((ull)mask4 << (x * 4ull));

	if (field[x] == 0)
	{
		field[x] |= block_line;
	}
	else
	{
		int p = bsr(field[x]);
		p /= 4;
		p += 1;

		if (block_line >= 16 && p >= HEIGHT - 1)//2つ落として、あふれるならば
		{
			return false;
		}
		if (block_line < 16 && p >= HEIGHT)//1つ落として、あふれるならば
		{
			return false;
		}

		*check |= ((ull)p << (x * 4ull));

		p *= 4;
		field[x] |= ((ull)block_line << p);
	}

	return true;
}
//ブロックを置く関数
bool Drop(int block, int pos, int rot, ull* check, ull field[WIDTH])
{

	int l = 0;
	int r = 0;

	switch (rot)
	{
	case 0:
		//1 2
		//3 4
		l = ((((block >> 0) & mask4) << 4) | ((block >> 8) & mask4));
		r = ((((block >> 4) & mask4) << 4) | ((block >> 12) & mask4));
		break;
	case 1:
		//3 1
		//4 2
		l = ((((block >> 8) & mask4) << 4) | ((block >> 12) & mask4));
		r = ((((block >> 0) & mask4) << 4) | ((block >> 4) & mask4));
		break;
	case 2:
		//4 3
		//2 1
		l = ((((block >> 12) & mask4) << 4) | ((block >> 4) & mask4));
		r = ((((block >> 8) & mask4) << 4) | ((block >> 0) & mask4));
		break;
	case 3:
		//2 4
		//1 3
		l = ((((block >> 4) & mask4) << 4) | ((block >> 0) & mask4));
		r = ((((block >> 12) & mask4) << 4) | ((block >> 8) & mask4));
		break;
	}

	if ((l & mask4) == 0) { l >>= 4; }//たとえばcase0において、l側のブロックの3が空マスだったら詰める
	if ((r & mask4) == 0) {r >>= 4;}

	*check = -1;
	if (!DropLine(pos, l, check, field)) {return false;}//ブロックを置けるか判定、checkの値は変更される
	if (!DropLine(pos + 1, r, check, field)) { return false; }

	return true;
}
//高さを求める
int GetHeight(int x, ull field[WIDTH])
{
	if (field[x] == 0)
	{
		return 0;
	}
	else
	{
		int p = bsr(field[x]);
		p /= 4;
		p += 1;
		return p;
	}
}

//ブロックを置く
void DropBlock(int x, int drop_y, int number, ull field[WIDTH])
{
	if (drop_y == 0)
	{
		field[x] |= number;
	}
	else
	{
		field[x] |= ((ull)number << (drop_y * 4));
	}
}
//空きマスを詰める関数
void DropBit(int x, ull field[WIDTH])
{
	int h = GetHeight(x,field);
	ull n = 0;
	int offset = 0;//空きマスが途中に何個あるか
	for (int i = 0; i < h; i++)
	{
		ull b = (field[x] & ((ull)mask4 << (i * 4)));//map[x]のi番目の高さまでの値をbに格納
		if (b == 0)//空きマスなら
		{
			offset += 4;
		}
		else
		{
			n |= (b >> offset);
		}
	}
	field[x] = n;
}
//消滅ブロックの格納と盤面最上段の更新
void SubmitSub(int* qx, int* qy, int* q_cnt, ull* next, int x, int y)
{
	qx[*q_cnt] = x;
	qy[*q_cnt] = y;
	(*q_cnt)++;
	int next_bottom = (((*next) >> (x * 4)) & mask4);
	if (next_bottom > y)//消滅ブロックのy座標がnextに予定されていた盤面最上段より小さければnextを更新
	{
		next_bottom = y;
		(*next) &= ~((ull)mask4 << (x * 4));
		(*next) |= ((ull)y << (x * 4));
	}
}
//チェイン数判定関数
int Submit(ull first_check, int* erase_cnt, int* erase_min_x, int* erase_max_x, ull field[WIDTH])
{
	int score = 0;
	int chein = 0;

	ull check = first_check;

	int ldx[] = { 1, 0, -1, 0, 1, 1, -1, -1 };
	int ldy[] = { 0, 1, 0, -1, 1, -1, 1, -1 };
	int qx[WIDTH * HEIGHT];
	int qy[WIDTH * HEIGHT];

	while (1)
	{
		ull next = -1;

		int q_cnt = 0;

		for (int x = *erase_min_x; x <= (*erase_max_x); x++)
		{
			int bottom = ((check >> (x * 4)) & mask4);
			//盤面が埋まってなければ
			if (bottom < mask4)
			{
				int h = GetHeight(x, field);
				//bottomはブロック設置前の盤面最上段、hはブロック設置後の盤面最上段
				for (int y = bottom; y < h; y++)
				{
					for (int d = 0; d < 8; d++)
					{
						int dx = x + ldx[d];
						int dy = y + ldy[d];
						if (!IsIn(dx, dy)) { continue; }

						int sum = Get(x, y, field) + Get(dx, dy, field);

						//qx,qyは消滅ブロックの保管配列、q_cntは消滅ブロックの個数、nextは盤面最上段
						if (sum == 10)
						{
							SubmitSub(qx, qy, &q_cnt, &next, x, y);
							SubmitSub(qx, qy, &q_cnt, &next, dx, dy);
						}
					}
				}
			}
		}

		if (q_cnt == 0) { break; }

		for (int i = 0; i < q_cnt; i++)
		{
			int x = qx[i];
			int y = qy[i];

			int shift = (y * 4);

			if (field[x] & ((ull)mask4 << shift))//盤面のx,y座標のブロックが0でないなら消滅するので、更新
			{
				(*erase_cnt)++;

				(*erase_min_x) = min(x, *erase_min_x);
				(*erase_max_x) = max(x, *erase_max_x);
			}

			field[x] &= (~((ull)mask4 << shift));//消滅が発生
		}

		for (int x = (*erase_min_x); x <= (*erase_max_x); x++)
		{
			DropBit(x,field);//落下処理
		}

		chein++;
		check = next;
		score += (int)floor(pow(1.3, chein));
	}

	return score;
}
//ブロックを置いて、チェイン数を判定する
int Put(int block, int pos, int rot,ull field[WIDTH])
{
	ull check;
	bool can_drop = Drop(block, pos, rot, &check, field);

	if (can_drop)
	{
		int erase_cnt;
		int erase_min_x = pos;
		int erase_max_x = pos + 1;

		int chain = Submit(check, &erase_cnt, &erase_min_x, &erase_max_x, field);

		return chain;
	}
	else
	{
		return -1;
	}
}
//盤面を出力
void Print(ull field[WIDTH])
{
	for (int y = HEIGHT - 1; y >= 0; y--)
	{
		for (int x = 0; x < WIDTH; x++)
		{
			cout << Get(x, y, field);
			cout << " ";
		}
		cout << endl;
	}
	cout << endl;
}
//お邪魔ブロックを降らせる
bool Ojama(ull field[WIDTH])
{
	bool is_end = false;
	for (int x = 0; x < WIDTH; x++)
	{
		if (field[x] == 0)
		{
			field[x] |= OJAMA;
		}
		else
		{
			int p = bsr(field[x]);
			p /= 4;
			p += 1;

			if (p >= HEIGHT)
			{
				is_end = true;
				continue;
			}

			p *= 4;
			field[x] |= ((ull)OJAMA << p);
		}
	}
	return is_end;
}
//ある条件下での仮想落下後のチェイン数を取得
int evaluate2_sub(int drop_x_start, int drop_x_end, int* erase_cnt, int* max_drop_x, int* erase_min_x, int* erase_max_x,ull field[WIDTH])
{
	int maxValue = 0;

	int _dx[] = { 1, 0, -1, 0, 1, 1, -1, -1 };
	int _dy[] = { 0, 1, 0, -1, 1, -1, 1, -1 };

	ull g_tempField[WIDTH];
	memcpy(g_tempField, field, sizeof(g_tempField));

	for (int x = drop_x_start; x <= drop_x_end; x++)
	{
		int drop_y = GetHeight(x,field);
		if (drop_y >= HEIGHT)
		{
			continue;
		}

		ull check = -1;
		check &= ~((ull)mask4 << (x * 4));
		check |= ((ull)drop_y << (x * 4));

		for (int n = 1; n <= 9; n++)
		{
			bool is_run = false;
			for (int d = 0; d < 8; d++)
			{
				int dx = x + _dx[d];
				int dy = drop_y + _dy[d];
				if (!IsIn(dx, dy)) { continue; }
				int sum = n + Get(dx, dy, field);
				if (sum == 10)
				{
					is_run = true;
					break;
				}
			}
			if (!is_run) {continue;}

			DropBlock(x, drop_y, n, field);

			int erase = 0;
			int min_x = x;
			int max_x = x;
			int score = Submit(check, &erase, &min_x, &max_x, field);
			if (maxValue < score)
			{
				maxValue = score;

				*max_drop_x = x;
				*erase_cnt = erase;
				*erase_min_x = min_x;
				*erase_max_x = max_x;
			}
			memcpy(field, g_tempField, sizeof(g_tempField));
		}
	}

	return maxValue;
}
//無条件下での仮想落下後のチェイン数を取得
int evaluate2(ull field[WIDTH])
{
	int erase_cnt;
	int max_drop_x;
	int erase_min_x;
	int erase_max_x;
	return evaluate2_sub(0, WIDTH - 1, &erase_cnt, &max_drop_x, &erase_min_x, &erase_max_x, field);
}
//盤面全てのブロックのある最上段を取得
int GetMaxY(ull field[WIDTH])
{
	int max_y = 0;
	for (int y = 0; y < HEIGHT; y++)
	{
		for (int x = 0; x < WIDTH; x++)
		{
			int n = Get(x, y, field);
			if (n > 0)
			{
				max_y = max(max_y, y);
			}
		}
	}
	return max_y;
}
//爆発スコアを返す
int evaluate(ull field[WIDTH])
{
	int _dx[] = { 1, 0, -1, 0, 1, 1, -1, -1 };
	int _dy[] = { 0, 1, 0, -1, 1, -1, 1, -1 };

	int cnt = 0;

	int score = 0;

	bool flg[WIDTH * HEIGHT] = {};

	ull g_tempField[WIDTH];

	memcpy(g_tempField, field, sizeof(g_tempField));

	int erase_min_x = 11;
	int erase_max_x = 0;

	for (int x = 0; x < WIDTH; x++)
	{
		for (int y = 0; y < HEIGHT; y++)
		{
			if (Get(x, y,field) == 5)
			{
				if (!flg[y * WIDTH + x])
				{
					flg[y * WIDTH + x] = true;
					cnt++;
					erase_max_x = max(erase_max_x, x);
					erase_min_x = min(erase_min_x, x);
				}
				for (int d = 0; d < 8; d++)
				{
					int dx = x + _dx[d];
					int dy = y + _dy[d];
					if (IsIn(dx, dy))
					{
						if (Get(dx, dy, field) != OJAMA && Get(dx, dy, field) > 0 && !flg[dy * WIDTH + dx])
						{
							flg[dy * WIDTH + dx] = true;
							cnt++;
							erase_max_x = max(erase_max_x, dx);
							erase_min_x = min(erase_min_x, dx);
						}
					}
				}
			}
		}
	}

	if (cnt == 0) { return 0; }

	score += (int)(floor(25 * pow(2, cnt / 12.0)));

	for (int x = erase_min_x; x <= erase_max_x; x++) {
		for (int y = 0; y < HEIGHT; y++) {
			if (flg[y * WIDTH + x]) {
				field[x] &= (~((ull)mask4 << (4 * y)));//消滅が発生
			}
		}
	}

	for (int x = erase_min_x; x <= erase_max_x; x++)
	{
		DropBit(x, field);//落下処理
	}

	int erase = 0;
	ull check = -1;

	for (int x = 0; x < WIDTH; x++) {
		if (field[x] != 0) {
			check &= ~((ull)mask4 << (x * 4ull));
			int p = bsr(field[x]);
			p /= 4;
			p += 1;
			check |= ((ull)p << (x * 4ull));
		}
	}

	score += Submit(check, &erase, &erase_min_x, &erase_max_x, field);

	memcpy(field, g_tempField, sizeof(g_tempField));

	return score;
}

/**
 * パック情報を読み込む
 * お邪魔が全ての埋まった状態のパックも用意しておく
 */
void readPackInfo(int g_packs[MAX_TURN]) {
	fprintf(stderr, "readPackInfo =>\n");
	int b[4];
	string _end_;

	for (int i = 0; i < MAX_TURN; i++) {
		cin >> b[0] >> b[1];
		cin >> b[2] >> b[3];
		cin >> _end_;
		g_packs[i] = 0;
		for (int j = 0; j < 4; j++)
		{
			g_packs[i] |= (b[j] << (j * 4));
		}
	}
}
/**
 * 1. ゲーム開始時の入力情報を読み込む
 * 2. フィールド情報を初期化しておく
 * 3. zoblish hash用の乱数テーブル生成
 * 4. パック情報の読み込み
 */
void init(ull g_myField[WIDTH], ull g_enemyField[WIDTH], ull g_field[WIDTH],int g_packs[MAX_TURN], int* beforeTime) {

	ull tmp1[WIDTH];

	memset(g_myField, EMPTY, sizeof(tmp1));
	memset(g_enemyField, EMPTY, sizeof(tmp1));
	memset(g_field, EMPTY, sizeof(tmp1));

	*beforeTime = 180000;

	readPackInfo(g_packs);
}
/**
 * ターン毎の情報を読み込む
 */
int readTurnInfo(int* beforeTime, int* myRemainTime, int* myOjamaStock, ull g_myField[WIDTH]
	, ull g_enemyField[WIDTH], int* g_scoreLimit, int* skillgauge, int* flag) {

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
	for (int y = 0; y < HEIGHT; y++)
	{
		for (int x = 0; x < WIDTH; x++)
		{
			ull n;
			cin >> n;
			g_myField[x] |= (n << ((BOTTOM - y) * 4));
			if (n != EMPTY) { block++; }
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
	for (int y = 0; y < HEIGHT; y++)
	{
		for (int x = 0; x < WIDTH; x++)
		{
			ull n;
			cin >> n;
			g_enemyField[x] |= (n << ((BOTTOM - y) * 4));
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

Action getBestAction2(int turn, ull g_field[WIDTH], int g_packs[MAX_TURN], int g_scoreLimit, int BEAM_WIDTH) {

	ull tmp1[WIDTH];

	Node root;
	memcpy(root.field, g_field, sizeof(tmp1));
	Action bestAction;
	int maxValue = -9999;

	deque<Node> que;
	que.push_back(root);

	unordered_map<ull, bool> checkNodeList;

	for (int depth = 0; depth < SEARCH_DEPTH; depth++) {
		int update = 0;
		int ks = (int)que.size();
#pragma omp parallel for reduction(+:update)
		for (int k = 0; k < ks; k++) {

			Node node = que[k];

			ull g_tempfield[WIDTH];

			memcpy(g_tempfield, node.field, sizeof(node.field));

			for (int x = 0; x <= 8; x++) {
				for (int rot = 0; rot < 4; rot++) {
					int score = Put(g_packs[turn + depth], x, rot, g_tempfield);
					if (score >= 0) {
						Node cand;
						cand.score = score;
						cand.value = cand.score;
						memcpy(cand.field, g_tempfield, sizeof(g_tempfield));
						cand.chain = (cand.score > 1 || (depth > 0 && cand.score > 0));
						if (!cand.chain) {
							int ev = evaluate2(g_tempfield);
							cand.value += ev;
							update++;
						}
						if (cand.score >= g_scoreLimit) { cand.value += 100 * cand.score; }
						cand.command = (depth == 0) ? COMMAND(x, rot) : node.command;
						fff[(36 * k) + (4 * x) + rot] = cand;
					}
					else {
						Node cand2;
						cand2.value = -114514;
						cand2.score = -114514;
						fff[(36 * k) + (4 * x) + rot] = cand2;
					}
					memcpy(g_tempfield, node.field, sizeof(node.field));
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
				ull hash = GetHash(node.field);

				if (!checkNodeList[hash] && (!node.chain || (update == 0 && node.score <= 2))) {
					checkNodeList[hash] = true;
					que.push_back(node);
				}
			}
		}
	}

	return bestAction;
}

/**
 * 一番良い操作を取得する
 *
 * @param [int] turn 今現在のターン
 * @return [Action] 一番ベストな行動情報
 */
Action getBestAction(int turn, ull g_field[WIDTH], int g_packs[MAX_TURN], int g_scoreLimit, int BEAM_WIDTH,int threshold) {

	ull tmp1[WIDTH];

	Node root;
	memcpy(root.field, g_field, sizeof(tmp1));
	Action bestAction;
	int maxValue = -9999;

	deque<Node> que;
	que.push_back(root);

	unordered_map<ull, bool> checkNodeList;

	for (int depth = 0; depth < SEARCH_DEPTH; depth++) {
		int update = 0;
		int ks = (int)que.size();

#pragma omp parallel for reduction(+:update)
		for (int k = 0; k < ks; k++) {

			Node node = que[k];

			ull g_tempfield[WIDTH];

			memcpy(g_tempfield, node.field, sizeof(node.field));

			for (int x = 0; x <= 8; x++) {
				for (int rot = 0; rot < 4; rot++) {
					int score = Put(g_packs[turn + depth], x, rot, g_tempfield);
						if (score >= 0) {
							Node cand;
							cand.score = score;
							cand.value = cand.score;
							memcpy(cand.field, g_tempfield, sizeof(g_tempfield));
							cand.chain = (cand.score != threshold);
							if (!cand.chain) {
								cand.value = evaluate(g_tempfield);
								update++;
							}
							//if (cand.value >= g_scoreLimit) { cand.value += 100 * cand.score; }
							cand.command = (depth == 0) ? COMMAND(x, rot) : node.command;
							fff[(36 * k) + (4 * x) + rot] = cand;
						}
						else {
							Node cand2;
							cand2.value = -114514;
							cand2.score = -114514;
							fff[(36 * k) + (4 * x) + rot] = cand2;
						}
					memcpy(g_tempfield, node.field, sizeof(node.field));
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
				ull hash = GetHash(node.field);

				if (!checkNodeList[hash] && (!node.chain || (update == 0 && node.score <= 2))) {
					checkNodeList[hash] = true;
					que.push_back(node);
				}
			}
		}
	}

	return bestAction;
}

/**
 * 自分のベストなコマンドを選択する
 *
 * @return [Action] 一番良いアクション
 */
Action getMyBestAction(int turn, ull g_field[WIDTH], ull g_myField[WIDTH], int myRemainTime
	, int g_packs[MAX_TURN], int g_scoreLimit, int threshold) {

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
		return getBestAction2(turn, g_field, g_packs, g_scoreLimit, BEAM_WIDTH);
	}
	else {
		return getBestAction(turn, g_field, g_packs, g_scoreLimit, BEAM_WIDTH, threshold);
	}
}
/**
 * AIのメインの処理部分
 *
 * @param [int] turn 現在のターン数
 */
void run(int turn, int* beforeTime, int* myRemainTime, int* myOjamaStock, int* g_scoreLimit, ull g_myField[WIDTH]
	, ull g_enemyField[WIDTH], int g_packs[MAX_TURN], ull g_field[WIDTH],int* skillgauge, int* flag) {

	int ut = readTurnInfo(beforeTime, myRemainTime, myOjamaStock, g_myField, g_enemyField, g_scoreLimit, skillgauge, flag);

	if (ut >= 20000) {
		cout << 15 << " " << 3 << endl;
		fflush(stderr);
		return;
	}
	if (*myOjamaStock >= 10) {
		Ojama(g_myField);
	}

	ull tmp[WIDTH];

	memcpy(tmp, g_myField, sizeof(tmp));

	Action action;

	if (*g_scoreLimit != 2 * EXPLODE_SCORE) {
		action = getMyBestAction(turn, g_field, g_myField, *myRemainTime, g_packs, *g_scoreLimit, -1);
	}
	else {

		int threshold = 0;

		if (*skillgauge < 80) { threshold = 1; }

		action = getMyBestAction(turn, g_field, g_myField, *myRemainTime, g_packs, *g_scoreLimit, threshold);
	}

	char command = action.command;
	int MNP = action.score;

	fprintf(stderr, "%2d: MNP=%d, FT=%d\n", turn, MNP, action.fireTurn);

	if (MNP >= 2 * FIRE_SCORE2 && turn == action.fireTurn) {
		*flag = -1;
	}

	int use_skill_score = evaluate(tmp);

	if (*skillgauge >= 80 && use_skill_score >= *g_scoreLimit) {
		cout << "S" << endl;
		fflush(stderr);
	}
	else {
		cout << X(command) << " " << ROT(command) << endl;
		fflush(stderr);
	}
}
int main() {

	cout << "tekitouk" << endl;

	int beforeTime;
	int myRemainTime;
	int myOjamaStock;
	int g_scoreLimit;
	int skillgauge;
	int flag = 0;

	ull g_myField[WIDTH] = { 0 };
	ull g_enemyField[WIDTH] = { 0 };
	ull g_field[WIDTH] = { 0 };

	int g_packs[MAX_TURN];

	init(g_myField, g_enemyField, g_field,g_packs, &beforeTime);

	for (int i = 0; i < MAX_TURN; i++) {
		memset(g_myField, 0, sizeof(g_myField));
		memset(g_enemyField, 0, sizeof(g_enemyField));
		memset(g_field, 0, sizeof(g_field));
		run(i, &beforeTime, &myRemainTime, &myOjamaStock, &g_scoreLimit, g_myField, g_enemyField, g_packs, g_field, &skillgauge, &flag);
	}

	return 0;
}