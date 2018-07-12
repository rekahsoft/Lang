/**
 * (C) Copyright Collin J. Doering 2017
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
 * File: fizzbuzz.js
 * Author: Collin J. Doering <collin@focus21.io>
 * Date: Jan 15, 2017
 */

// Naive implementation
function fizzbuzz_naive(a, b) {
    if (a > b) {
        throw "Invalid interval error!"
    }

    for (let i = a; i <= b; ++i) {
        let divBy3 = i % 3 === 0;
        let divBy5 = i % 5 === 0;
        if (divBy3 && divBy5) {
            console.log('FizzBuzz');
        } else if (divBy3) {
            console.log('Fizz');
        } else if (divBy5) {
            console.log('Buzz');
        } else {
            console.log(i);
        }
    }
        
}

//fizzbuzz_naive(0, 100);

// Generates a array of integers from a to b
function rangeArr(a, b) {
    if (a > b) {
        throw 'Invalid range error!';
    }
    let ret = [];
    for (let i = a; i <= b; ret.push(i++));
    return ret;
}

function printArr(arr) {
    arr.forEach((a) => console.log(a));
}

function fizzbuzz_functional(x, y) {
    const fuzzbuzz_f = (a, b) => rangeArr(a, b).map((i) => {
        let divBy3 = i % 3 === 0;
        let divBy5 = i % 5 === 0;
        if (divBy3 && divBy5) {
            return 'FizzBuzz';
        } else if (divBy3) {
            return 'Fizz';
        } else if (divBy5) {
            return 'Buzz';
        } else {
            return i;
        }        
    });

    printArr(fuzzbuzz_f(x, y));
}

//fizzbuzz_functional(0, 100);


// Abstracted fizzbuzz
function fizzbuzz_functional_abstracted(a, b, ...xs) {
    return rangeArr(a, b).map((i) => {
        return xs.reduce((acc, x) => {
            let ret = acc;
            if (i % x.num === 0) {
                ret = typeof ret === 'number' ? x.name : ret.concat(x.name);
            }
            return ret;
        }, i);
    });
}

printArr(fizzbuzz_functional_abstracted(0, 100, { name: 'Fizz', num: 3 }, { name: 'Buzz', num: 5 }, { name: 'Bam', num: 8 }));

// Abstracted and using generators
function take(gen) {
    return (n) => {
        let ret = [];
        for (let i = 0; i < n; ++i)
            ret.push(gen.next().value);
        return ret;
    };
}

function* range(a, b) {
    if (a > b) {
        throw 'Invalid range error!';
    }

    for (let i = a; i <= b; yield i++);
}

function* fizzbuzz(a, b, ...xs) {
    let r = range(a, b);

    for (i of r) {
        yield xs.reduce((acc, x) => {
            let ret = acc;
            if (i % x.num === 0) {
                ret = typeof ret === 'number' ? x.name : ret.concat(x.name);
            }
            return ret;
        }, i);
    }
}

let fbb = fizzbuzz(0, 100000000000, { name: 'Fizz', num: 3 }, { name: 'Buzz', num: 5 }, { name: 'Bam', num: 8 });
printArr(take(fbb)(10));

for (i of fbb) {
     console.log(i);
}
