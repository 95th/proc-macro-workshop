// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use seq::seq;

seq!(N in 1..100 {
    #(
        fn f#N () -> u64 {
            #( N + )* 2
        }
    )*
});

fn main() {
    println!("{}", f1());
}
