#include <cassert>
#include <tuple>
#include "List.h"
using namespace std;

enum ItemType { Plus, Minus, Num };
struct Item
{
    ItemType _type;
    int _num;
    Item(int i) : _type(Num), _num(i) {}
    Item(ItemType t) : _type(t), _num(-1) {}
};

std::ostream& operator<<(std::ostream& os, Item const & it)
{
    switch (it._type)
    {
    case Plus: os << "+"; break;
    case Minus: os << "-"; break;
    case Num: os << it._num; break;
    }
    return os;
}

using State = List<Item>;

// To retrieve A from Plan<A> pl
// decltype(pl(State()).first)
template<class A>
using Plan = function<pair<A, State>(State)>;

template<class A>
pair<A, State> runPlan(Plan<A> pl, State s)
{
    return pl(s);
}

template<class A>
A evalPlan(Plan<A> pl, State s)
{
    return runPlan(pl, s).first;
}

template<class A>
Plan<A> mreturn(A a)
{
    return [a](State s) { return make_pair(a, s); };
}

template<class A, class F>
auto mbind(Plan<A> pl, F k) -> decltype(k(pl(State()).first))
{
    using B = decltype(k(pl(State()).first)(State()).first);
    static_assert(std::is_convertible<
        F, std::function<Plan<B>(A) >> ::value,
        "mbind requires a function type Plan<B>(A)");

    return [pl, k](State s) {
        pair<A, State> ps = runPlan(pl, s);
        Plan<B> plB = k(ps.first);
        return runPlan(plB, ps.second);
    };
}

// A version of mbind with a continuation that ignores its argument
template<class A, class F>
auto mthen(Plan<A> pl, F k) -> decltype(k())
{
    using B = decltype(k()(State()).first);
    static_assert(std::is_convertible<
        F, std::function<Plan<B>() >> ::value,
        "mthen requires a function type Plan<B>()");

    return [pl, k](State s) {
        pair<A, State> ps = pl(s);
        // ignore ps.first
        Plan<B> plB = k();
        return runPlan(plB, ps.second);
    };
}

Plan<State> getState()
{
    return [](State s) { return make_pair(s, s); };
}

Plan<void*> putState(State newState)
{
    return [newState](State s) { return make_pair(nullptr, newState); };
}

Plan<Item> pop()
{
    return mbind(getState(), [](State s) {
        Item it = s.front();
        return mthen(putState(s.popped_front()), [=]() {
            return mreturn(it);
        });
    });
}

Plan<int> calc();

Plan<int> add()
{
    return mbind(calc(), [](int n) {
        return mbind(calc(), [=](int m) {
            return mreturn(n + m);
        });
    });
}

Plan<int> subtract()
{
    return mbind(calc(), [](int n) {
        return mbind(calc(), [=](int m) {
            return mreturn(n - m);
        });
    });
}

Plan<int> calc()
{
    return mbind(pop(), [](Item it) {
        switch (it._type)
        {
        case Plus:  return add();
        case Minus: return subtract();
        case Num:   return mreturn(it._num);
        };
    });
}

template<class S, class A>
std::ostream& operator<<(std::ostream& os, pair<A, S> const & p)
{
    os << "(";
    os << p.first << ", " << p.second;
    os << ")";
    return os;
}

void test()
{
    List<Item> stack{ Item(Plus), Item(Minus), Item(4), Item(8), Item(3) };
    cout << evalPlan(calc(), stack) << endl;
}

int main()
{
    test();
    return 0;
}
