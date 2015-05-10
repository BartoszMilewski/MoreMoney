#include <cassert>
#include <tuple>
#include "List.h"
using namespace std;

using State = List<int>;

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
    return runState(pl, s).first;
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
        F, std::function<Plan<B>(A)>> ::value,
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
        F, std::function<Plan<B>()>> ::value,
        "mthen requires a function type Plan<B>()");

    return [pl, k](State s) {
        pair<A, State> ps = pl(s);
        // ignore ps.first
        Plan<B> plB = k();
        return runPlan(plB, ps.second);
    };
}

template<class S, class A>
std::ostream& operator<<(std::ostream& os, pair<A, S> const & p)
{
    os << "(";
    os << p.first << ", " << p.second;
    os << ")";
    return os;
}

pair<int, List<int>> select(List<int> lst)
{
    int i = lst.front();
    return make_pair(i, lst.popped_front());
}

Plan<int> sel = &select;

int main()
{
    List<int> st{ 1, 2, 3 };
    Plan<pair<int, int>> pl = 
        mbind(sel, [=](int i) { return
        mbind(sel, [=](int j) { return
        mreturn(make_pair(i, j));
    });
    });
    cout << runPlan(pl, st) << endl;
    return 0;
}
