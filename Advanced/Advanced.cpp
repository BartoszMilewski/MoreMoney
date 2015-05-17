#include "StateList.h"
#include "RBMap.h"
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

// select ::[a] ->[(a, [a])]
// select[] = []
// select(x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <-select xs]

template<class A>
PairList<A> select(List<A> lst)
{
    if (lst.isEmpty())
        return PairList<A>();

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

int asNumber(vector<int> const & v)
{
    int acc = 0;
    for (auto i : v)
    {
        acc = 10 * acc + i;
    }
    return acc;
}

using Map = RBMap<char, int>;

int get(Map const & subst, char c)
{
    return subst.findWithDefault(-1, c);
}

int toNumber(Map const & subst, string const & str)
{
    int acc = 0;
    for (char c : str)
    {
        int d = get(subst, c);
        acc = 10 * acc + d;
    }
    return acc;
}

// eliminate all duplicate characters
string nub(string const & str)
{
    string result;
    for (char c : str)
    {
        if (result.find(c) == string::npos)
            result.push_back(c);
    }
    return result;
}

/*
solve :: StateList [Int] (Int, Int, Int)
solve = StateList select >>= go (nub "sendmoremoney") M.empty
  where
    go [c] subst i = prune (M.insert c i subst)
    go (c:cs) subst i = StateList select >>= go cs (M.insert c i subst)
    prune subst = do
        guard (get 's' /= 0 && get 'm' /= 0)
        let send  = toNumber "send"
            more  = toNumber "more"
            money = toNumber "money"
        guard $ send + more == money
        return (send, more, money)
      where
        get c = fromJust (M.lookup c subst)
        toNumber str = asNumber (map get str)
        asNumber = foldl (\t o -> t*10 + o) 0
*/

StateList<tuple<int, int, int>> prune(Map subst)
{
    return mthen(guard(get(subst, 's') != 0 && get(subst, 'm') != 0), [=]() {
        int send = toNumber(subst, "send");
        int more = toNumber(subst, "more");
        int money = toNumber(subst, "money");
        return mthen(guard(send + more == money), [=]() {
            return mreturn(make_tuple(send, more, money));
        });
    });
}

// go[c] subst i = prune(M.insert c i subst)
// go(c:cs) subst i = StateList select >>= go cs(M.insert c i subst)
StateList<tuple<int, int, int>> go(string str, Map subst, int i)
{
    StateList<int> sel = &select<int>;

    assert(str.length() > 0);
    if (str.length() == 1)
        return prune(subst.inserted(str[0], i));
    else
    {
        return mbind(sel, [=](int n) {
            string tail(&str[1], &str[str.length()]);
            return go(tail, subst.inserted(str[0], i), n);
        });
    }
}

// solve = StateList select >>= go (nub "sendmoremoney") M.empty

StateList<tuple<int, int, int>> solve()
{
    StateList<int> sel = &select<int>;
    Map subst;
    return mbind(sel, [=](int s) {
        return go(nub("sendmoremoney"), subst, s);
    });
}

int main()
{
    List<int> lst{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    cout << evalStateList(solve(), lst);
    return 0;
}