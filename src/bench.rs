extern crate test;

use test::{Bencher, black_box};

use crate::{interpreter::{tests::evaluate, Interpreter}, source::SourceFile};

#[bench]
fn bench_startup(b: &mut Bencher) {
    b.iter(|| black_box({
        evaluate("null").unwrap()
    }));
}

#[bench]
fn bench_factorial(b: &mut Bencher) {
    let mut interpreter = Interpreter::new(None).unwrap();
    b.iter(|| black_box({
        interpreter.parse_and_evaluate(SourceFile::new_temp("
            impl Integer {
                func benchFactorial {
                    self lessThanOrEquals: 1 $
                        ifTrue: [ 1 ]
                        else:   [ (self - 1) benchFactorial $ * self ]
                }
            }
            
            20 benchFactorial        
        ").rc()).unwrap();
    }));
}

#[bench]
fn bench_iteration(b: &mut Bencher) {
    let mut interpreter = Interpreter::new(None).unwrap();
    b.iter(|| black_box({
        interpreter.parse_and_evaluate(SourceFile::new_temp("
            sum = 0.
            10000 times: [
                sum = sum + 1.
            ]
        ").rc()).unwrap()
    }));
}

