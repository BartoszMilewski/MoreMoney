#if ! defined(LIST_H)
#define LIST_H

#include <cassert>
#include <memory>
#include <functional>
#include <initializer_list>
#include <iostream> // print

template<class T> class FwdListIter;

template<class T>
class List
{
    struct Item
    {
        Item(T v, std::shared_ptr<const Item> tail)
        : _val(v), _next(std::move(tail))
        {}
        // singleton
        explicit Item(T v) : _val(v) {}
        T _val;
        std::shared_ptr<const Item> _next;
    };
    friend Item;
    explicit List(std::shared_ptr<const Item> items)
        : _head(std::move(items)) {}
public:
    // Empty list
    List() {}
    // Cons
    List(T v, List const & tail)
        : _head(std::make_shared<Item>(v, tail._head)) {}
    // Singleton
    explicit List(T v) : _head(std::make_shared<Item>(v)) {}
    // From initializer list
    List(std::initializer_list<T> init)
    {
        for (auto it = std::rbegin(init); it != std::rend(init); ++it)
        {
            _head = std::make_shared<Item>(*it, _head);
        }
    }

    bool isEmpty() const { return !_head; }
    T front() const
    {
        assert(!isEmpty());
        return _head->_val;
    }
    List popped_front() const
    {
        assert(!isEmpty());
        return List(_head->_next);
    }
    // Additional utilities
    List pushed_front(T v) const
    {
        return List(v, *this);
    }
    List take(int n)
    {
        if (n <= 0 || isEmpty()) return List();
        return popped_front().take(n - 1).pushed_front(front());
    }
private:
    std::shared_ptr<const Item> _head;
};

template<class T, class F>
auto fmap(F f, List<T> lst) -> List<decltype(f(lst.front()))>
{
    using U = decltype(f(lst.front()));
    static_assert(std::is_convertible<F, std::function<U(T)>>::value,
        "fmap requires a function type U(T)");
    List<U> result;
    forEach(lst, [&](T x) {
        result = result.pushed_front(f(x));
    });
    return result;
}

template<class T>
List<T> concat(List<T> const & a, List<T> const & b)
{
    if (a.isEmpty())
        return b;
    return List<T>(a.front(), concat(a.popped_front(), b));
}

template<class T>
List<T> concatAll(List<List<T>> const & xss)
{
    List<T> result;
    forEach(xss, [&](List<T> const & xs){
        forEach(xs, [&](T const & e) {
            result = result.pushed_front(e);
        });
    });
    return result;
}

// consumes the list when called: 
// forEach(std::move(lst), f);

template<class T, class F>
void forEach(List<T> lst, F f)
{
    static_assert(std::is_convertible<F, std::function<void(T)>>::value,
        "forEach requires a function type void(T)");
    while (!lst.isEmpty()) {
        f(lst.front());
        lst = lst.popped_front();
    }
}

template<class T>
std::ostream& operator<<(std::ostream& os, List<T> const & lst)
{
    os << "[";
    forEach(lst, [&os](T v) {
        os << v << " ";
    });
    os << "]";
    return os;
}

#endif
