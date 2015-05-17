#include "List.h"
#include <functional>
#include <tuple>

using namespace std;

using State = List<int>;

// newtype StateList s a = StateList (s -> [(a, s)])
template<class A>
using PairList = List<pair<A, State>>;

template<class A> 
using StateList = function<PairList<A>(State)>;

// runStateList :: StateList s a -> s -> [(a, s)]
// runStateList(StateList g) s = g s

template<class A>
PairList<A> runStateList(StateList<A> st, State s)
{
    return st(s);
}

// evalStateList::StateList s a->s ->[a]
// evalStateList(StateList g) s = fmap fst(g s)
template<class A>
List<A> evalStateList(StateList<A> st, State s)
{
    return fmap([](pair<A, State> const & p)->A {
        return p.first;
    }, st(s));
}

// return x = StateList(\s ->[(x, s)])
template<class A>
StateList<A> mreturn(A a)
{
    return [a](State s) { return PairList<A>(make_pair(a, s)); };
}

// (StateList g) >>= k = StateList(\s->concat $ fmap(\(a, s') -> runStateList (k a) s') (g s))

// k is a function<StateList<S, B>(A)>
// mbind returns StateList<S, B> 

template<class A, class F>
auto mbind(StateList<A> g, F k) -> decltype(k(g(State()).front().first))
{
    return [g, k](State s) {
        PairList<A> plst = g(s);
        //List<PairList<B>> 
        auto lst2 = fmap([k](pair<A, State> const & p) {
            A a = p.first;
            State s1 = p.second;
            auto ka = k(a);
            auto result = runStateList(ka, s1);
            return result;
        }, plst);
        return concatAll(lst2);
    };
}

// A version of mbind with a continuation that ignores its argument
template<class A, class F>
auto mthen(StateList<A> g, F k) -> decltype(k())
{
    return [g, k](State s) {
        PairList<A> plst = g(s);
        auto lst2 = fmap([k](pair<A, State> const & p) {
            State s1 = p.second;
            auto ka = k();
            auto result = runStateList(ka, s1);
            return result;
        }, plst);
        return concatAll(lst2);
    };
}

// mzero = StateList(\s ->[])
template<class A>
StateList<A> mzero()
{
    return[](State s) {
        return PairList<A>();
    };
}

StateList<void*> guard(bool b)
{
    if (b) {
        return [](State s) {
            return List<pair<void*, State>>(make_pair(nullptr, s));
        };
    }
    else
        return mzero<void*>();
}

template<class S, class A>
std::ostream& operator<<(std::ostream& os, pair<A, S> const & p)
{
    os << "(";
    os << p.first << ", " << p.second;
    os << ")";
    return os;
}

template<class A, class B, class C>
std::ostream& operator<<(std::ostream& os, tuple<A, B, C> const & t)
{
    os << "(";
    os << get<0>(t) << ", " << get<1>(t) << ", " << get<2>(t);
    os << ")";
    return os;
}

