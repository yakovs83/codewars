#![allow(dead_code)]

// Task: Given a list of digits, return the smallest number that could be formed from these digits, using the digits only once ( ignore duplicates).
// Note: Only positive integers will be passed to the function (> 0 ), no negatives or zeros.
fn min_value(mut digits: Vec<i32>) -> i32 {
    digits.sort();
    digits.dedup();
    digits.iter().fold(0, |acc, x| acc * 10 + x)
}

#[test]
fn min_value_test() {
    assert_eq!(min_value(vec![1, 3, 1]), 13);
    assert_eq!(min_value(vec![4, 7, 5, 7]), 457);
    assert_eq!(min_value(vec![4, 8, 1, 4]), 148);
}

// Your task is to make a function that can take any non-negative integer as a argument and return it with its digits in descending order. Essentially, rearrange the digits to create the highest possible number.
// Example:
// Input: 21445 Output: 54421

fn descending_order(x: u64) -> u64 {
    let mut n = x;
    let mut digits = Vec::new();
    while n != 0 {
        digits.push(n % 10);
        n /= 10;
    }
    digits.sort();
    digits.iter().rev().fold(0, |acc, x| acc * 10 + x)
}

#[test]
fn test_descending_order() {
    assert_eq!(descending_order(0), 0);
    assert_eq!(descending_order(1), 1);
    assert_eq!(descending_order(15), 51);
    assert_eq!(descending_order(1021), 2110);
    assert_eq!(descending_order(123456789), 987654321);
    assert_eq!(descending_order(145263), 654321);
    assert_eq!(descending_order(1254859723), 9875543221);
}

// A number m of the form 10x + y is divisible by 7 if and only if x − 2y is divisible by 7
// Your task is to return to the function seven(m) (m integer >= 0) an array
// (or a pair, depending on the language) of numbers, the first being the last number m with at
// most 2 digits obtained by your function (this last m will be divisible or not by 7),
// the second one being the number of steps to get the result.
fn seven(n: i64) -> (i64, i32) {
    fn seven_rec(n: i64, i: i32) -> (i64, i32) {
        if n >= 100 {
            seven_rec(n / 10 - (n % 10) * 2, i + 1)
        } else {
            (n, i)
        }
    }
    seven_rec(n, 0)
}

#[test]
fn test_seven() {
    assert_eq!(seven(477557101), (28, 7));
    assert_eq!(seven(477557102), (47, 7));
    assert_eq!(seven(1603), (7, 2));
}
