extern crate test;

use test::{Bencher, black_box};

use crate::interpreter::tests::evaluate;

#[bench]
fn bench_startup(b: &mut Bencher) {
    b.iter(|| black_box({
        evaluate("null").unwrap()
    }));
}

#[bench]
fn bench_factorial(b: &mut Bencher) {
    b.iter(|| black_box({
        evaluate("
            impl Integer {
                func benchFactorial {
                    self lessThanOrEquals: 1 $
                        ifTrue: [ 1 ]
                        else:   [ (self sub: 1) benchFactorial $ mul: self ]
                }
            }
            
            20 benchFactorial        
        ").unwrap()
    }));
}

#[bench]
fn bench_iteration(b: &mut Bencher) {
    b.iter(|| black_box({
        evaluate("
            sum = 0.
            10000 times: [
                sum = sum add: 1.
            ]
        ").unwrap()
    }));
}

