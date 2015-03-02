/**
 * (C) Copyright Collin J. Doering 2015
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * File: rdm.js
 * Author: Collin J. Doering <collin.doering@rekahsoft.ca>
 * Date: Feb  2, 2015
 */

var church = (function () {
    var spec,
        t = function (x) {
            var ret = function (y) {
                return x;
            };
            return ret;
        },
        f = function (x) {
            var ret = function (y) {
                return y;
            };
            return ret;
        },
        church_zero = function (f) {
            var ret = function (x) {
                return x;
            };
            return ret;
        },
        nil;

    // ------------------------------
    // Church Boolean implementation
    // ------------------------------

    function make_bool (b) {
        if (b) {
            return t;
        } else {
            return f;
        }
    }

    function unchurch_bool (b) {
        return b(true)(false);
    }

    function and (x) {
        var ret = function (y) {
            return x(y)(x);
        };
        return ret;
    }

    function or (x) {
        var ret = function (y) {
            return x(x)(y);
        };
        return ret;
    }

    function not (x) {
        var ret = function (y) {
            var aret = function (z) {
                return x(z)(y);
            };
            return aret;
        };
        return ret;
    }

    function xor (x) {
        var ret = function (y) {
            return x(not(y))(y);
        };
        return ret;
    }

    function church_if (p) {
        var ret = function (a) {
            var aret = function (b) {
                return p(a)(b);
            };
            return aret;
        };
        return ret;
    }

    // -----------------------
    // Church natural numbers
    // -----------------------

    function succ (n) {
        var ret = function (f) {
            var aret = function (x) {
                return f(n(f)(x));
            };
            return aret;
        };
        return ret;
    }

    function add (n) {
        var ret = function (m) {
            var aret = function (f) {
                var bret = function (x) {
                    return n(f)(m(f)(x));
                };
                return bret;
            };
            return aret;
        };
        return ret;
    }

    function mult (n) {
        var ret = function (m) {
            var aret = function (f) {
                var bret = function (x) {
                    return n(m(f))(x);
                };
                return bret;
            };
            return aret;
        };
        return ret;
    }

    function expt (n) {
        var ret = function (m) {
            var aret = function (f) {
                var bret = function (x) {
                    return (m(n))(f)(x);
                };
                return bret;
            };
            return aret;
        };
        return ret;
    }

    function isZero (n) {
        var ret = n(function (x) {
            return f;
        })(t);
        return ret;
    }

    function make_nat (n) {
        var i, ret = church_zero.bind({});
        for (i = 0; i < n; i += 1) {
            ret = succ(ret);
        }
        return ret;
    }

    function unchurch_nat (n) {
        var ret = function (i) {
            var aret = function (m) {
                i += 1;
                return i;
            };
            return aret;
        }, i = 0;
        return n(ret(i))(0);
    }

    // -------------
    // Church Pairs
    // -------------

    function make_pair (a) {
        var ret = function (b) {
            var aret = function (f) {
                return f(a)(b);
            };
            return aret;
        };
        return ret;
    }

    function fst (p) {
        return p(t);
    }

    function snd (p) {
        return p(f);
    }

    function unchurch_pair (p) {
        return [fst(p), snd(p)];
    }

    // -------------
    // Church Lists
    // -------------

    function make_list () {}

    function unchurch_list (xs) {}

    nil = make_pair(t)(t);
    isNil = fst;

    function cons (h) {
        var ret = function (t) {
            return make_pair(f)(make_pair(h)(t));
        };
        return ret;
    }

    function head (l) {
        return fst(snd(l));
    }

    function tail (l) {
        return snd(snd(l));
    }

    // -----------------
    // The Y Combinator
    // -----------------
    // * This doesn't work as javascript is strictly evaluated
    // -----------------
    
    function fix (g) {
        var f = function (x) {
            return g(x(x));
        };
        return f(f);
    }

    // Setup specification object
    spec = {
        "if": church_if,
        fix: fix,
        bool: {
            make: make_bool,
            toNative: unchurch_bool,
            t: t,
            f: f,
            not: not,
            and: and,
            or: or,
            xor: xor
        },
        nat: {
            make: make_nat,
            toNative: unchurch_nat,
            zero: church_zero,
            succ: succ,
            add: add,
            mult: mult,
            expt: expt,
            isZero: isZero
        },
        pair: {
            make: make_pair,
            toNative: unchurch_pair,
            fst: fst,
            snd: snd
        },
        list: {
            make: make_list,
            toNative: unchurch_list,
            nil: nil,
            isNil: isNil,
            cons: cons,
            head: head,
            tail: tail
        }
    };
    return spec;
})();

// -------------------------

function unchurch_church_nat_test (lim) {
    var i,
        lim = lim || 12;
    for (i = 0; i <= lim; i += 1) {
        if (i !== church.nat.toNative(church.nat.make(i))) {
            console.log('Failed church.nat.toNative(church.nat.make(' + i + '))');
            return false;
        }
    }
    console.log('Created church nats from 1 to ' + lim + ' successfully.');
    return true;
}

function uncurry (f) {
    var ret = function (x, y) {
        return f(x)(y);
    };
    return ret;
}

function matrix_test (f, g, n, tp) {
    var i, j,
        t = tp || 100,
        name = n || "unnamed";
    for (i = 0; i <= t; i += 1) {
        for (j = 0; j <= t; j += 1) {
            if (f(i,j) !== g(i,j)) {
                console.log('Failed matrix test for ' + name + ' with inputs ' + i + ' and ' + j + '.');
                return false;
            }
        }
    }
    console.log('Successfully completed test for ' + name + '.');
    return true;
}

unchurch_church_nat_test(100);

matrix_test(function (a, b) {
    return a + b;
}, function (a, b) {
    return church.nat.toNative(church.nat.add(church.nat.make(a))(church.nat.make(b)));
}, "Test church addition");

// matrix_test(function (a, b) {
//     return a - b;
// }, function (a, b) {
//     return church.nat.toNative(minus(church.nat.make(a))(church.nat.make(b)));
// });

matrix_test(function (a, b) {
    return a * b;
}, function (a, b) {
    return church.nat.toNative(church.nat.mult(church.nat.make(a))(church.nat.make(b)));
}, "Test church multiplication");

// very slow for some reason? (not that this implementation will ever be)
matrix_test(function (a, b) {
    return Math.pow(a, b);
}, function (a, b) {
    return church.nat.toNative(church.nat.expt(church.nat.make(a))(church.nat.make(b)));
}, "Test church exponentiation", 7);


// -------------------------------------------------------
// Factorial function written similar to how the y-combinator works
function factorial (x) {
    var g = function (h) {
        var f = function (n) {
            if (n < 2) {
                return 1;
            } else {
                return n * h(h)(n - 1);
            }
        };
        return f;
    };
    return g(g)(x);
}

var i;
for (i = 0; i < 10; i += 1) {
    console.log(factorial(i));
}
