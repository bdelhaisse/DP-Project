# Declarative Programming (Prolog)

This repository contains the project that I did in August 2015 for the ["Declarative Programming" course](http://ai.vub.ac.be/~ydehauwe/index.php?page=decl_prog) teached by Prof. De Hauwere at the *Vrije Universiteit Brussel*. This course followed the ["Simply Logical: Intelligent Reasoning by Example"](http://www.cs.bris.ac.uk/~flach/SimplyLogical.html) book. The goal of the course was to understand the declarative programming paradigm and how it contrasts with the imperative paradigm.

To code in Prolog, the following interpreter is recommended: [SWI-Prolog](http://www.swi-prolog.org/)


## Project: Task/Job Scheduler Problem

The goal of this project was to implement an offline scheduler for job/task parallel computations on multiple instances using the Prolog programming languages. This famous optimization problem is also known as the [Job Scheduling Problem](https://en.wikipedia.org/wiki/Job_shop_scheduling). Ten instances divided in two groups (the small and large instances) were provided to test the performance of our scheduler. 

The following table gives an overview of the instances provided (retranscribed here in case the [project webpage](https://ai.vub.ac.be/node/1353) is removed):

| #  | name               | #cores | #tasks | dependencies? | heteregeneous? | communication? | optimal | speedup |
|----|--------------------|--------|--------|---------------|----------------|----------------|---------|---------|
| 1  | batch_small_homo   | 4      | 7      | no            | no             | no             | 100     | 4       |
| 2  | batch_large_homo   | 16     | 320    | no            | no             | no             | ???     | ???     |
| 3  | batch_small_hetero | 4      | 7      | no            | yes            | no             | 100     | 3.6     |
| 4  | batch_large_hetero | 16     | 320    | no            | yes            | no             | ???     | ???     |
| 5  | fib_small_nc       | 4      | 7      | yes           | no             | no             | 50      | 1.4     |
| 6  | fib_large_nc       | 8      | 265    | yes           | no             | no             | ???     | ???     |
| 7  | fib_small_uc       | 4      | 7      | yes           | no             | yes            | 60      | 1.167   |
| 8  | fib_large_uc       | 8      | 265    | yes           | no             | yes            | ???     | ???     |
| 9  | sor_small          | 4      | 6      | yes           | yes            | yes            | 174     | 1.425   |
| 10 | sor_large          | 4      | 251    | yes           | yes            | yes            | ???     | ???     |


More details about the project can be found on the [project webpage](https://ai.vub.ac.be/node/1353).


## How to run the code?

Here is an example on how to launch the Prolog code in the SWI-Prolog interpreter.

First, navigate to the src/ folder and launch the SWI-Prolog interpreter by typing in the terminal:
```bash
swipl
```

Then, in the SWI-Prolog interpreter, you can for instance test the following example:
```prolog
?- [project]. % load the project
true.

?- load(instances/batch_small_homo). % load the 'batch_small_homo' dataset
true.

?- getSolution(S). % call one of the implemented fct in 'project.pl'. Type 'Tab' to get other valid solutions.
S = solution([schedule(c1, [t1, t2, t3, t4, t5, t6|...]), schedule(c2, []), schedule(c3, []), schedule(c4, [])]) ;
S = solution([schedule(c1, [t1, t2, t3, t4, t5, t6]), schedule(c2, [t7]), schedule(c3, []), schedule(c4, [])]) ;
...

?- find_optimal(S). % call another implemented fct to get the optimal solution
S = solution([schedule(c1, [t1]), schedule(c2, [t2, t7]), schedule(c3, [t3, t6]), schedule(c4, [t4, t5])]).

?- execution_time(solution([schedule(c1, [t1]), schedule(c2, [t2, t7]), schedule(c3, [t3, t6]), schedule(c4, [t4, t5])]), ET). % get the execution time of the given solution
ET = 100.

?- pretty_print(solution([schedule(c1, [t1]), schedule(c2, [t2, t7]), schedule(c3, [t3, t6]), schedule(c4, [t4, t5])])). % print nicely the solution in the interpreter
The solution is:
On the core c1: [t1]
On the core c2: [t2,t7]
On the core c3: [t3,t6]
On the core c4: [t4,t5]
Total Execution Time: 100 ms.
true.

?- unload(instances/batch_small_homo). % unload the dataset
true.

?- halt. % to exit the prolog interpreter
```

### Personal notes

This project was by far one of the most troubling and challenging project that I had to do. Not because of the problem itself as I knew how to solve it in the imperative/object-oriented paradigm using heuristic algorithms, but because I had to solve it in the declarative paradigm. Compared to the functional paradigm that I learned the same year using Scheme, it took me more time to get used to this new way of thinking. Anyway, I learned a lot from this project and it was very rewarding at the end.

Side note: this project can still be improved by implementing heuristic algorithms to get faster and better solutions.