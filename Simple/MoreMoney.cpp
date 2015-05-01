#include "StateList.h"
#include <vector>
using namespace std;

// select ::[a] ->[(a, [a])]
// select[] = []
// select(x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <-select xs]

template<class A>
PList<A> select(List<A> lst)
{
    if (lst.isEmpty())
        return PList<A>();

    A       x = lst.front();
    List<A> xs = lst.popped_front();

    auto result = List<pair<A, State>>();
    forEach(select(xs), [x, &result](pair<A, List<A>> const & p)
    {
        A       y = p.first;
        List<A> ys = p.second;
        auto y_xys = make_pair(y, ys.pushed_front(x));
        result = result.pushed_front(y_xys);
    });

    return result.pushed_front(make_pair(x, xs));
}

/*
// asNumber :: [Int] -> Int
// asNumber = foldl(\t o->t * 10 + o) 0
int asNumber(List<int> const & lst)
{
    return foldl([](int acc, int d) {
        return acc * 10 + d;
    }, 0, lst);
}
*/
int asNumber(vector<int> const & v)
{
    int acc = 0;
    for (auto i : v)
    {
        acc = 10 * acc + i;
    }
    return acc;
}


StateL<pair<int, int>> testBind()
{
    StateL<int> st = &select<int>;
    return mbind(st, [st](int x) {
        return mbind(st, [x](int y) {
            return mreturn(make_pair(x, y));
        });
    });
}

StateL<int> testThen()
{
    return mthen(mzero<int>(), []() {
        cout << "Ignoring\n";
        return mreturn(42);
    });
}

StateL<int> testGuard()
{
    StateL<int> st = &select<int>;
    return mbind(st, [](int x) {
        return mthen(guard(x > 2), [x]() {
            return mreturn(x);
        });
    });

}

    //s <-StateL select
    //e <-StateL select
    //n <-StateL select
    //d <-StateL select
    //m <-StateL select
    //o <-StateL select
    //r <-StateL select
    //y <-StateL select
    //guard $ s /= 0 && m /= 0
    //let send = asNumber[s, e, n, d]
    //more = asNumber[m, o, r, e]
    //money = asNumber[m, o, n, e, y]
    //guard $ send + more == money
    //return (send, more, money)


StateL<tuple<int, int, int>> solve()
{
    StateL<int> sel = &select<int>;

    return mbind(sel, [=](int s) {
    return mbind(sel, [=](int e) {
    return mbind(sel, [=](int n) {
    return mbind(sel, [=](int d) {
    return mbind(sel, [=](int m) {
    return mbind(sel, [=](int o) {
    return mbind(sel, [=](int r) {
    return mbind(sel, [=](int y) {
        return mthen(guard(s != 0 && m != 0), [=]() {
            int send  = asNumber(vector<int>{s, e, n, d});
            int more  = asNumber(vector<int>{m, o, r, e});
            int money = asNumber(vector<int>{m, o, n, e, y});
            return mthen(guard(send + more == money), [=]() {
                return mreturn(make_tuple(send, more, money));
            });
        });
    });});});});});});});});
}



int main()
{
    List<int> lst{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    /*
    StateL<int> st = &select<int>;
    PList<int> sel = runStateL(st, lst);
    cout << sel;
    cout << endl;
    cout << evalStateL(st, lst);
    cout << endl;
    cout << runStateL(mreturn<int>(42), lst);
    cout << endl;
    cout << evalStateL(testBind(), lst);
    cout << endl;
    cout << evalStateL(testThen(), lst);
    cout << endl;
    cout << evalStateL(testGuard(), lst);
    cout << endl;
    cout << asNumber(lst);
    cout << endl;
    */
    cout << evalStateL(solve(), lst);
    return 0;
}