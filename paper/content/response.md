Dear Editor,

thank you for the thorough reviews. We would like to use this document to provide a line-by-line response to the reviewers' comments.

> The reviewers generally agree that some technically sound work has been done, but they seriously question the necessity of having an arrow interface: one reviewer say it's not surprising, the other reviewer observes that the examples mostly force use of the pure function or Kleisli instance of arrows (meaning that they are just syntax on top of a monadic structure). 
> Moreover, IMHO the textual parts also need improvement: sometimes they are very terse, sometimes the line of attention jumps forth and back between the main text and the appendix, as also indicated by one of the reviewers.
> 
> Hence, I contend that significant work is needed on carving out the motivation for having the arrow interface (just "look, I can do it" is not enough) and also on smoothing out the nooks in the text. The reviewers suggest you resubmit and are all willing to review a reworked version of the paper.
> 
> Please note that resubmitting your manuscript does not guarantee eventual acceptance, and that your resubmission will be subject to re-review by the referees before a decision is rendered.

> __Referee: 1__
> Comments:
> 
> Overall the paper leaves a mixed impression. 
> 
> Of course, it is an interesting exercise to use arrows for parallel programming.
> In this sense the paper does a good job in explaining the background from the
> concept of arrows to the three parallel Haskell variants and then step by step
> lays out its story. On the way so many papers are referenced that a side 
> contribution of the paper would be the annotated bibliograpyhy for the topics
> concerned.
>

Thank you.

> But when approaching the final steps of the paper one question became 
> increasingly pending:  So what?
> 
> What is the real underlying contribution of the paper, in the sense of novel 
> insight, beyond the foreground contributions summarised in the paper?
>

TODO

Our formalism inspired S. Zhang's Session Arrows (Zhang, 2019).

> After all, it wasn't particularly difficult or challenging to use arrows for 
> parallel computing. Assuming the authors are right that no-one tried before
> them, and I have no reason to doubt this, maybe simply no-one has found it
> worthwhile so far. At least, I found the pure "it can be done" view on the
> contribution not entirely convincing. A relevant question, for me, would be
> whether the arrow-based approach makes parallel computing with Haskell
> in any sense simpler or more accessible to a larger audience than the 
> existing solutions, preferably beyond a pure matter of taste. This aspect is,
> unless I have overlooked it, not discussed in the paper, and so the whole 
> story gets a bit lost in technicalities.
> 
> One argument that the proposed interface is non-monadic could be considered
> an advantage over the Par monad, but neither over GpH nor over Eden, as 
> rightfully discussed in the conclusion. 
> 
> I agree that a common API across parallel Haskell variants has the obvious
> principle advantages, but that would hold for any sort of common API no matter
> how good or bad, with arrows or without.

There are two sides to our contribution. Firstly, we provide a common API to parallel Haskells, regardless its nature. Such an interface makes various approaches to parallelism in Haskell more comparable. Programs and benchmarks written on top of the common API would be more portable. Secondly, we did an intended design choice for such an API and used Arrows for it. Such approach appled more to us than an implementation of a further parallel Haskell using Arrows.

> The skeletons and benchmarks are all adopted from existing publications about
> the three parallel Haskell variants and merely ported to the arrow-based API.

We had multiple goals when designing the benchmarks. One of them was to be able to benchmark the Arrow-based API itself, i.e., to compare the performance of an Arrow-based benchmark with a non-Arrow benchmark. Naturally, we chose existing non-Arrow implementations to achieve this goal.

> The final observation that the proposed approach comes with marginal if any
> measurable overhead is at first glance good news. In fact, the opposite would
> surely be bad news and obviously question the entire approach. However, the
> results also demonstrate what the proposed solution effectively is: hardly 
> more than a thin layer of abstraction over three well established and 
> carefully engineered parallel implementations of Haskell.

If we are allowed to summarize, here you basically critique the shallowness of the API. However, as performance was vigilantly kept in mind during the design and implementation of PArrows, we consider this shallow layer a valuable contribution. With an abstraction combining Kleisli functions and pure functions in one notion, we are able to bridge conceptually very different flavors of parallel Haskells. And do so efficiently.

> ***************************************************************************
> Suggestions:
> 
> The paper is generally well written, but I nonetheless have a few 
> suggestions for improvement and minor corrections.
> 
> page 2:
> 
> "Further implementations ... are viable, too": What is this supposed to 
> precisely mean? I would assume that many implementations would be somewhat
> "viable". If you have accomplished concrete steps in the directions of
> HdpH or Frege, I suggest to explain that in concrete terms. As is, the text
> is too hand-waiving.

We adjusted the wording and made more clear, that we conducted some experiments with those further implementations, but do not focus on them in this work.

> 
> page 4:
> 
> "out of A list of Arrows"

Adjusted.

> 
> page 5:
> 
> Like on page 2, the diversion towards Frege remains cryptic.
>

We clarified the wording.

> page 7:
> 
> Provide a brief characterisation of profunctors, or at least a reference.
> "since they they allow"

We rephrased the offending paragraph, providing an additional reference explaining Profunctors (Boisseau & Gibbons, 2018). The text emphases now that it is possible to define Arrows deriving from Profunctors, as Appendix B shows. There we also cite Asada (2010).

> 
> The introductory text of section 3.2 (before 3.2.1) is mostly re-iterating 
> aspects already discussed in previous sections, partially with identical
> phrasing. I suggest to cut this short.

The text was removed.

> 
> Other side issues like LVish or HdpH should have been discussed earlier.
> The background section should really focus on the relevant background,
> whereas the selection of background topics should be clear at this point.
>

We moved the text ob LVish and HdpH to the related work.

> page 10:
> 
> "efficiency OR the lack thereof" maybe?

Fixed.

> 
> page 11:
> 
> The dashes should be longer, or in Latex terms: "---".

Fixed globally.

> 
> page 13:
> 
> "instances of the ArrowParallel instances" ?

This was obviously a writing mishap, fixed.

> 
> page 15:
> 
> What is meant be the term "implementation agnostic"? Any (high-level) 
> programming approach is somewhat implementation-agnostic.

We clarified the wording and emphasised portability across parallel Haskells.

> 
> page 16ff:
> 
> The text flow continuously jumps to the appendix and back; numerous forward
> references to figures in the appendix are increasingly annoying for the 
> reader. I believe that the authors should make a choice: if some example,
> implementation, illustration, etc, is relevant for the reader then it should
> appear immediately and not in the appendix. Everything else should either be
> skipped or the remaining contents of the appendix should be explained once.
>

TODO

> page 16:
> 
> "toplogical"

Fixed.

> 
> page 19:
> 
> The question arising on the top of page 19 should be investigated. The statement
> that such differences would be beyond the scope of the paper appears a bit lame.
>

We changed the wording.

Our reasoning is that the behaviour in question arises from the subtle details of laziness and demand in Eden standard library. We argue that such details are not of major interest to the generic JFP audience. Our paper focuses on establishing a common interface between different parallel Haskells, with a minor workaround the above demand issue becomes irrelevant.

> page 22:
> 
> "repeating": why is the front part of the word set in italics?

We were referencing the `repeat` function. We now use an apostrophe for more clarity.

> 
> page 23:
> 
> "using the Gentleman algorithm" -> "using Gentleman's algorithm"

Fixed.

> 
> page 24:
> 
> If the purpose is to compare Figures 29 and 30, I strongly suggest to make
> sure they appear on the same page, or better the graphs are combined in a 
> single figure. A related question is: why should there be any observable
> difference? After all the exact same infrastructure and almost the same
> implementation are used.

We forced both figures on a single page.

The point of those figures is exactly to showcase that there is no observable difference, despite using a further intermediate abstraction layer in form of our API.

> 
> page 25:
> 
> I find the statement that "PArrows hold up in terms of performance when 
> compared with the original parallel Haskells" a bit strong. At the end
> of the day, PArrows is not a new parallel Haskell, but merely an abstraction
> wrapper around the existing parallel Haskells. 
> 
> It is of little surprise that 32 hyperthreaded cores do not behave exactly as
> 16 real cores. Still the conclusion to disregard hyperthreading because it
> behaves differently appears illogic. I'm inclined to agree that hyperthreading
> might not provide any particular interesting insights for the questions
> concerned, but the justification should be rephrased.
> 
> page 27:
> 
> The text in 7.1.4 and the beginning of 7.2 should be condensed.

We implemented this change request.

> 
> page 28:
> 
> A graphical illustration of the experimental findings appears mandatory.
> The mentioning of a zillion number is pretty unreadable and complicates
> comprehension of the results. The text should be used to explain the most
> interesting numbers and discuss the findings.

TODO

> 
> The term "speedup" is used extensively without ever explaining the base line.

We added a definition of speedup.

> 
> page 30:
> 
> The absolute runtimes on 16 cores are fairly small. This raises the question
> whether larger problem sizes or simply repeated computing of the existing
> problem sizes could contribute to noise reduction and might possibly lead to
> more reliable experimental results.
> 
> "extensible formalism THAT can be easily ported"

Fixed.

> 
> The sentence "We argue that ..." is pretty much repeated right thereafter.

We removed this sentence all together.

> 
> page 32ff:
> 
> The references contain numerous capitalisation errors. For conferences
> there is an unmotivated but consistent space between the acronym and the year.

The space is removed.

> 
> 
> ***************************************************************************
> 
> 
> 
> 
> Referee: 2
> 
> Comments to the Author
> Summary:
> 
> This paper introduces the use of Arrows for specifying parallelism. In
> particular, it defines an Arrow-based domain specific language
> (library) and implements it using multiple parallel Haskells.
> 
> More specifically, it introduces specialized Arrows as a general
> interface to parallel computations, by defining the ArrowParallel 
> type class and giving instance implementations for GpH, the Par Monad 
> and Eden. To demonstrate its expressiveness, it shows that the
> Parallel Arrows can be used to define some useful algorithmic
> skeletons that abstract typical parallel computations. The
> experimental results (on four examples) shows the minor overhead 
> induced by PArrows, which is outweighed by their uniform and general
> interface. 
> 
> Overall Evaluation:
> 
> This paper shows a useful development of a general arrow-based
> interface for specifying parallel computation in Haskell. This
> development is definitely worth doing (and I would wish an earlier
> development of such arrow-based interface).
> 
> The whole paper is clearly written and well structured. It explains
> not only the background and related work to make the paper
> self-contained, but also the motivation and the details of the
> arrow-based language for parallel programming in Haskell and the three
> instance implementations.
> 
> I like the paper and enjoyed reading it, but the *technical*
> contributions of this paper sounds limited with the standard of JFP.
> 
> I agree with the contributions listed in the introduction, but I am
> wondering how challenging it is to have these contributions. Following
> the research work on arrow-interfaces for GUI, inverse computation or
> functional reactive computation, it does not look difficult to define
> such an arrow-based interface for parallel programming for functional
> parallel programming. It is my hope that the authors can clarify the
> challenges and how these challenges are tacked in the later revision.
> 
> Detailed Comments:
> 
> Section 1:
> 
> On attractive point of this arrow-based general interface is that it
> allows to parallel implementation at will. It seems that it would be
> impossible for this switching unless every parallel computing
> primitive is wrapped with the general interface. Is the arrow-based
> language defined in this paper sufficient?  Or in other words, is
> there any parallelism in the current parallel Haskell system (say GhP)
> that cannot be described in this arrow-based interface?
>

We would expect some at least technical difficulties with Accelerate. However, our interface seems to be fairly universal. A very different approach is data parallel programming. Our approach is task parallel.

> Section 3:
> 
> For GpH, can you describe the two primitives par/seq in terms of
> PArrows?

Our approach is directly reversed: we define PArrows in terms of par/seq. However, TODO TODO

> 
> Section 4:
> 
> How general is the new interface language? I see that "parEvalN" in
> Section 4 needs to be extended to deal with "loop" later in Section
> 4. Do we need to extend it again to deal with other cases? Can you
> give a clear definition of the interface language, together with its
> application scope (including discussion what it cannot do)?
> 
> Section 5:
> 
> I got a bit lost when reading the "Future" class, and I cannot fully
> understand why it can support direct communications between
> nodes. What is the relationship between "Future" and "parEvalN"? Is
> "Future" part of PArrows?

It is not part of the class holding parEvalN, but it extends the general interface of PArrows.

> 
> Section 6:
> 
> At the beginning of this section, it says "Now we have developed
> Parallel Arrows far enough to .... While there are many possible
> skeletons to implement, we demonstrate the expressive power of PArrows
> here using four map-based and three toplogical skeletons."  Later in
> Section 6.2, it shows that the original developed PArrows are not good
> enough and should to be extended to deal with "loop". Does this mean
> that PArrows defined in this paper can be the basis for the future
> extension?
> 
> Referee: 3
> 
> Comments to the Author
> # Paper summary
> 
> There are now a large number of Haskell APIs and runtimes for parallel
> computing, from shared memory on a single machine to distributed
> computation. Programmers currently must choose a particular API and
> program to it, making it difficult to experiment with alternative
> parallelism solutions.
> 
> This paper proposes a common API, based on Hughes'
> Arrows. Implementations of this API for GpH, the Par Monad, and Eden
> are given. The authors demonstrate a number of programs written using
> this API, and present some benchmarks to show that using a common API
> does not introduce any significant overhead.
> 
> # Assessment
> 
> The authors present a (mostly) nice looking API for writing parallel
> programs in Haskell, that can be supported by at least three different
> backends, and has low overhead.
> 
> My problem with this paper is that I can't see why Arrows are
> essential. Since the title is "Arrows for Parallel Computation", this
> is a major problem.

TODO

> 
> Why don't I think Arrows are essential to this work? The authors
> present the ArrowParallel type class (Sec 4.1, pg 11):
> 
>    class Arrow arr => ArrowParallel arr a b conf where
>       parEvalN :: conf -> [arr a b] -> arr [a] [b]
> 
> and then give instances for this class for GpH, the Par monad, and
> Eden. The former two permit any Arrow instance that is also an
> ArrowChoice, but Eden only permits the (->) and Kleisli
> instances.
> 
> However, as far as I can tell, all the actual example parallel
> programs presented only use the (->) instance (Figure 28 seems to be
> the only serious program presented). Are there any examples of using
> other Arrow instances? Is there any point in using anything other than
> the (->) and Kleisli instances if they don't work with Eden?

It would be possible to creature further special instances for Eden, if those are of interest.

> 
> The code in Figure 9 shows that a 'parEvalN' for monadic computations
> is derivable from one for pure arrows. So one could present the
> library as:
> 
>    class ParListEval arr a b conf where
>      parEvalN :: conf -> [a -> b] -> [a] -> [b]
> 
> and use the 'conf' parameter as a way of selecting between
> implementations as well as passing in configuration parameters. The
> Kleisli instance, if it is needed, can now be written as (following
> Fig 9):
> 
>    parEvalNM :: (Monad m, ParListEval arr a (m b) conf) => conf -> [a -> m b] -> [a] -> m [b]
>    parEvalNM conf fs as = sequence (parEvalN conf fs as)
> 
> Just using plain functions rather than the Arrow combinators would
> probably also lead to much easier to read code, especially when it
> comes to writing the topological skeletons in Section 6.
> 
> Now, it would be an interesting observation that the three parallelism
> libraries considered here can be unified behind this
> interface. However, given that the whole set up that this paper
> proposes seems unnecessary, I recommend reject and resubmit.
> 
> If there were compelling examples of uses of non `(->)` arrows for
> writing parallel programs, then I'd be happier with this paper.
> 
> ## Other comments
> 
> - The `parEvalN` function is the central contribution of this paper,
>  but very little is specified about how it should work, beyond "in
>  parallel" and what is specified by the types. For instance, what
>  laziness behaviour is expected of this function? (this is very
>  important for the looping examples later). What happens if the two
>  input lists have different lengths? Since the Eden implementation is
>  precisely Eden's `spawnF`, then aren't you in effect just settling
>  for whatever specification Eden provides?
> 
> - It is necessary to add a facility for 'Futures' to organise
>  inter-node communication more effectively. This is perhaps
>  necessary, but it spoils the "backend agnostic" approach -- if one
>  is testing on GpH and then deploying to Eden then adding Futures to
>  a GpH targeted implementation is just noise. On the other hand, it
>  is good that they are semantically "the identity", and do not affect
>  the overall result.
> 
>  I found the introduction of Futures in Section 5 quite difficult to
>  understand until I guessed that they are effectively 'handles' that
>  are passed around until 'get' retrieves the actual data -- this
>  isn't explained! There is also no explanation of how Futures fix the
>  problem with the combinator in Figure 13 (why is it called
>  'someCombinator' and not 'outlineCombinator'?)
> 
> - The ArrowLoopParallel type class on page 19. The only reason for
>  this type class is to paper over differences in evaluation order
>  between GpH and the Par Monad, and Eden. For a journal paper, I
>  would have expected some more investigation into the cause of the
>  discrepancy between the backends. Does the Eden implementation force
>  more of the input lists in one go? Is there a smallest example that
>  shows the problem?
> 
>    - Since you are in control of the implementations of parEvalN for GpH
>      and Par, then is it not possible to alter them to match Eden?
> 
>    - The fact that the programmer has to remember where to use
>      `parEvalN`, `loopParEvalN` and `postLoopParEvalN` is a burden,
>      and it seems that if they mix it up, it will depend on the
>      backend whether or not the program will crash. It also affects
>      the composability of the library. What if someone wants to use,
>      e.g., the `torus` skeleton inside another `loop`?
> 
>    - Am I right in thinking that `postLoopParEvalN` is always used
>      with `(repeat (arr id))` (except in `simplePipe`)? So it is
>      being used to insert parallelism after the loop? If this is the
>      pattern always being used, then why not provide a special `loop`
>      combinator that does the right thing for parallel arrows? For
>      example:
> 
>            loopPar conf f = loop f >>> parEvalN conf (repeat (arr id))
> 
>      for GpH and the Par monad, and
> 
>            loopPar conf f = loop f
> 
>      for Eden (this would remove any overhead of mapping `id` over
>      everything afterwards in Eden). The programmer would still have
>      to remember to use `loopParEvalN` inside the loop though.
> 
> - The further examples of using parEvalN to reconstruct known
>  parallelism strategies and skeletons are also unsatisfying. The
>  Heterogenous tasks in 4.3.2 rely on the use of partial functions
>  like 'head' to work, and indicate that the parEvalN interface is
>  perhaps too lax with types and that perhaps an interface like:
> 
>        parEval2 :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
> 
>  could be provided as well as parEvalN?
> 
> - The implementations of topological skeletons in Section 6.2 start to
>  get quite involved quite quickly, due to the use of laziness, and
>  I'm not convinced that the Arrow interface is really helpful
>  here. If they were written just using functions they'd look like
>  normal Haskell functions, albeit ones that have quite subtle
>  laziness behaviour. For instance, pipeSimple can be rewritten as the
>  considerably simpler looking:
> 
>        pipeSimple conf fs a =
>           let xs = parEvalN conf fs (lazy (a : tail xs)) in last (a:xs)
> 
>  this works with the following definition of `parEvalN` for GpH:
> 
>        parEvalN fs xs = evalN fs xs `using` parList rseq
> 
>  (which also has the advantage of (a) working on the empty list of
>  functions; and (b) running the pipeline in parallel, unlike in the
>  GpH instance in the paper due to its instance of
>  `ArrowLoopParallel`). I think the other examples, `pipe2`, `ring`, and
>  `torus` would also benefit from not using the Arrow
>  interface. (unless I'm wrong that there are other useful Arrow
>  instances for ParallelArrow?)
> 
>  I think the subtle uses of laziness in these definitions also
>  requires some discussion -- for instance the dataflow going in
>  `pipeSimple`, why is this loop OK?
> 
> - The benchmarks don't cover all the skeletons discussed. In
>  particular, the pipe and ring skeletons are not covered by any of
>  the four benchmarks. Since the whole point of parallelisation is to
>  make programs go faster, it would seem to me that a particular
>  construct in a paper about parallelism should only appear if it can
>  be shown to actually make programs go faster.

The `pipe` and `ring` skeletons are needed, as they allow for a more explainable and didactic introduction of the `torus`. Basically, `ring` is a warped `pipe`, and `torus` is a `ring` of `ring`s. Hence, the `torus` benchmark is the ultimate test for the whole family of `pipe`, `ring`, and `torus`.

> 
> - A frustrating amount of the code is placed in the appendix (mainly
>  appendix C) and referenced from the main text. It would be much
>  easier to read the paper if all of the code was inlined in the
>  text. In the present state, the reader has to keep flipping to the
>  end to find the implementations of crucial bits of code (e.g.,
>  `lazy`).
> 
> - Throughout, the writing is a bit sloppy. I've highlighted some
>  examples below.
> 
> ## Text comments
> 
> ### Page 2
> 
> Para 1, line 3: "short" => "for short"
> 
> Para 2, line 1: "A key" => "The key"
> 
> Para 3: line 1: "Arrows" => "Arrow"
> 
> Para 3: line 3: "we define a shallow-embedded DSL for Arrows". Aren't
> you defining a shallowly embedded DSL for parallel programming,
> *using* Arrows?
> 
> Para 3: line 6: "very" is vague.

We fixed the above spelling problems, thank you very much.

> 
> Para 4: line 2: is there a missing 'Arrow arr' constraint on the type
> signature?
>

Indeed, we fixed this mistake and apologize.

> Para 6: line 1: this is the first mention of "PArrow" -- it should be
> defined earlier.

We fixed this.

> 
> ### Page 3
> 
> Para 1: line 1: "This has many practical advantages". Are there any
> others beyond being able to switch backend easily?

TODO

> 
> Para 6: line 2: "parallelisations" => "execution strategies"

Thank you for this suggestion. We accepted it, the final wording is: "... relatively easy to define alternate execution strategies, including parallel evaluation."
 
> Para 6: line 7: "is completely orthogonal to our approach". This seems
> a bit defensive. But since you bring it up, then why not explain why
> it is orthogonal?

Accelerate focuses on accessing the GPU and adapts to the notions of Cuda language. We found it hard to combine the concepts of PArrows and Accelerate under one hood. However, we are considering to use PArrows to orchestrate multiple Accelerate instances.

We reason that such an in-depth explanation is going to far in the paper text, hence we omit it.

> 
> Para 8: line 2: Should be a colon after "following"?

Fixed.

> 
> Para 8: line 3: I'm not sure what you mean by "communication-centred"
> in relation to LVars?

TODO

> 
> ### Page 4
> 
> The section on Algorithmic skeletons needs a forward reference to
> later in the paper where you discuss how to express skeletons using
> your formalism.
> 
> "Arrows" para: "less restrictive alternative", well, it is less
> restrictive for the writer of the arrow, but not for the user.
>

TODO

> "Arrow": line 10: "capsule the computation", do you mean "encapsulate
> the computation"? This whole sentence seems quite vague anyway. I'd
> characterise Liu et al's work as providing a specific implementation
> of arrows that allows for optimisation of arrows with feedback by
> retaining some intensional information about the computation. The
> comparison with you work confuses me: you say that their arrows allow
> parallel composition, but then seem to say that yours is different?
> What is a thing that you can express but they can't?
>

We adjusted the description of Lue et al.'s work.

The difference between our work and Lue et al. is that they would allow for composition of their arrows in parallel. We provide operators to execute a set of arrows in parallel (yielding an arrow again).

WHAT'S THE DIFFERENCE

> "Arrow": line 14: missing "a" before "list".
> 
> "Arrow": line 15: "different" => "differently"
> 
> "Arrow": line 16: "several tasks in robotics". This is ambiguous, do
> you mean several different sorts of tasks that a robot might be asked
> to do separately, or orchestrate the tasks at runtime?
>

XXXXXXXXXXXXXXXXX

> Another potential piece of related work is Turon's Reagents, that are
> based on an Arrow-like interface (albeit for concurrency, not
> parallelism): https://people.mpi-sws.org/~turon/reagents.pdf
>

Thank you for this reference, we include it now.

> ### Page 5
> 
> Para 1: Line 2: "which is basically"; better "A Haskell-like language
> for the JVM"

We adapted your wording.

> 
> Para 2; line 2: "typical GUI tasks" => you could be less vague.

Fixed.

> 
> Sec 3.1, para 1, last line: why not use the typeclass constraint
> "Monad m =>" instead of saying "where m is a Monad"?

Fixed.
 
> Sec 3.1, para 2, first line: What does the "This" at the end of the
> line refer to? Also, there is no mention of the Arrow laws.
>

XXXXXXXXXXXXXXXXX

> Sec 3.1, para 3: "syntactic sugar". I don't think these derived
> combinators should be called syntactic sugar, because you aren't
> defining the syntax of a language anywhere. They are either "derived
> combinators", or more prosaically, "utility functions".

We changed the way we reference those combinators throughout the text.
> 
> ### Page 6
> 
> The diagramatic representations of arrows on this page and the next
> are nice, but they don't seem to be used anywhere else. The diagrams
> in the text seem to be a mixture of communication topologies, mappings
> from input to output locations, and these arrow diagrams.
> 
> `ArrowApply`, `ArrowLoop` and `ArrowChoice` aren't defined anywhere -- how much
> background knowledge about Arrows do you expect the reader to have?

XXXXXXXXXXXXXXXXXXX


> "we have a truly more general interface as compared to a monadic one"
> -- but this isn't true for Eden right? And there are no examples in
> the paper of using non `(->)` arrows to write programs, unless I've
> missed one?

TODO: monadic RabinMiller!

> 
> ### Page 7
> 
> You need some reference to where Profunctors are defined, or at least
> a short introduction to them.

Fixed, as mentioned above.

> 
> "Among the most important are probably" ... "probably" is a vague. You
> could just say "the ones we focus on are...". Also, this sentence is
> sort of claiming that you chose these ones because they are the "most
> important", but the first sentence on the next page gives reasons in
> terms of the features of each one.
>

We have severely shortened and rewritten this paragraph alltogether, now there is no mention of importance at this point.

> ### Page 9
> 
> You could write `parEvalN` for the Par monad more concisely as:
> 
>    parEvalN fs as = runPar $ mapM (return >>> spawn >>= get) $ zipWith ($) fs as

Thank you very much, we replace the definition.

> 
> ### Page 10
> 
> You don't explain what the `Trans` typeclass is. `Trans`portable?

Yes, specialization of packing and unpacking and network transmission, e.g. streams for lists. We added two sentences explaining this issue.

> 
> ### Page 11
> 
> Why not put the `evalN` definition in the text for the reader?
> 
> ### Page 12
> 
> Figure 7: why not use a `newtype` for `Conf`?
> 
> Why is `Conf` for GpH in the figure (Fig 7), but not for the Par Monad
> instance (Fig 8)?
> 
> ### Page 13
> 
> Figure 9: does the second instance need the `Trans b` constraint?
> Should the `conf` before the `where` be a `Conf`?
>

XXXXXXXXXXXXXXXXXXX

> ### Page 14
> 
> Figure 11: what is the `ChunkSize` type? `Int`?

Yes. We added an explanation.

> 
> Where is the `ArrowApply` constraint used? Is it in `chunksOf` -- I
> can't find a definition of this function in the paper.
> 
> Section 4.3.2: the readbility of this subsection would be greatly
> improved by having the code of the function you are talking about!
> 
> ### Page 16
> 
> As I said above, Section 5 doesn't really explain how Futures fix the
> problem.

XXXXXXXXXXXXXX

> 
> ### Page 17
> 
> Fig 17, caption: "Other than" => "Unlike"

Fixed.

> 
> ### Page 18
> 
> second last line: remove the final "the"

Fixed.

> 
> ### Page 19
> 
> first line: remove "when"

Fixed.

> 
> ### Page 20
> 
> paragraph 3: "probably": do some benchmarks?

This was more of an speculation on what is the better in the API than a solid performance claim. We adjusted the wording correspondingly.

> 
> last line: "send" => "sent"

Fixed.

> 
> ### Page 22
> 
> "This combinator can, for example, be used to calculate the shortest
> paths in a graph using Warshall's algorithm." Have you done this?
> Could it be included in the benchmarks?
> 
> ### Page 23
> 
> Figure 27 is where the code starts to get really unreadable. If you
> are going to stick with Arrows, then why not use Paterson's arrow
> notation? Or give a diagramatic representation to help the reader?
> 
> ### Page 25
> 
> "We disregarded the hyper-threading ability in most of the cases."
> There should be a forward reference to the section discussing hyper
> threading specifically.
>

XXXXXXXXXXXXXXXXXX

> ### Page 37
> 
> last line: "as" => "was"

Fixed.

> 
> ### Page 38
> 
> The type signature for `(|||)` seems messed up (the ArrowChoice
> typeclass constraint is muddled up with the type of the first
> argument).

TODO TODO TODO