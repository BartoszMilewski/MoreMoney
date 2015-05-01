#include "List.h"
#include <functional>
#include <tuple>

using namespace std;

// newtype StateL s a = StateL (s -> [(a, s)])
template<class State, class A>
using PList = List<pair<A, State>>;

template<class State, class A>
using StateL = function<PList<State, A>(State)>;

// runStateL :: StateL s a -> s -> [(a, s)]
// runStateL(StateL g) s = g s

template<class State, class A>
PList<State, A> runStateL(StateL<State, A> st, State s)
{
    return st(s);
}

// evalStateL::StateL s a->s ->[a]
// evalStateL(StateL g) s = fmap fst(g s)
template<class State, class A>
List<A> evalStateL(StateL<State, A> st, State s)
{
    return fmap([](pair<A, State> const & p)->A {
        return p.first;
    }, st(s));
}

// return x = StateL(\s ->[(x, s)])
template<class State, class A>
StateL<State, A> mreturn(A a)
{
    return [a](State s) { return PList<State, A>(make_pair(a, s)); };
}

// (StateL g) >>= k = StateL(\s->concat $ fmap(\(a, s') -> runStateL (k a) s') (g s))

// k is a function<StateL<S, B>(A)>
// mbind returns StateL<S, B> 

template<class State, class A, class F>
auto mbind(StateL<State, A> g, F k) -> decltype(k(g(State()).front().first))
{
    return [g, k](State s) {
        PList<State, A> plst = g(s);
        //List<PList<B>> 
        auto lst2 = fmap([k](pair<A, State> const & p) {
            A a = p.first;
            State s1 = p.second;
            auto ka = k(a);
            auto result = runStateL(ka, s1);
            return result;
        }, plst);
        return concatAll(lst2);
    };
}

// A version of mbind with a continuation that ignores its argument
template<class State, class A, class F>
auto mthen(StateL<State, A> g, F k) -> decltype(k())
{
    return [g, k](State s) {
        PList<State, A> plst = g(s);
        auto lst2 = fmap([k](pair<A, State> const & p) {
            State s1 = p.second;
            auto ka = k();
            auto result = runStateL(ka, s1);
            return result;
        }, plst);
        return concatAll(lst2);
    };
}

// mzero = StateL(\s ->[])
template<class State, class A>
StateL<State, A> mzero()
{
    return[](State s) {
        return PList<State, A>();
    };
}

template<class State>
StateL<State, void*> guard(bool b)
{
    if (b) {
        return [](State s) {
            return List<pair<void*, State>>(make_pair(nullptr, s));
        };
    }
    else
        return mzero<State, void*>();
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

