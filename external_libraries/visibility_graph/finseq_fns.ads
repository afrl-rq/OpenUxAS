with Finite_Sequences;

generic
   type T is private;
   Default_Value: T;
package Finseq_Fns with SPARK_Mode is

   -- Import the finite_sequences package
   package FS is new Finite_Sequences(T => T, Default_Value => Default_Value);
   use FS;

   -- Singleton sequence
   -- PVS: singleton_seq(e: T): {fs: finseq[T] | length(fs) = 1} = (: e :);
   function Singleton_Seq(E : T) return Finseq is
      (Length => 1, Seq => (0 => E, others => Default_Value));

   -- Nonempty sequence type
   -- PVS: nonempty_seq: TYPE = {s: finseq[T] | slength > 0};
   subtype Nonempty_Seq is Finseq with Dynamic_Predicate => Nonempty_Seq.Length > 0;

   -- CAR (first element)
   -- PVS: car(seq: nonempty_seq): T = seqseq(0);
   function Car(Seq : Nonempty_Seq) return T is (Seq.Seq(0));

   -- CDR (all but the first element)
   -- PVS: cdr(seq: nonempty_seq): finseq[T] =
   --   (# length := seq`length - 1,
   --      seq := LAMBDA(i: below(seq`length - 1)): seq`seq(i + 1) #);
   function Cdr(Seq : Nonempty_Seq) return Finseq;

   -- Prepend an element to a sequence
   -- PVS: prepend(e: T, seq: finseq[T]): finseq[T] = singleton_seq(e) o seq;
   function Prepend(E : T; Seq : Finseq) return Finseq is (Concat (Singleton_Seq (E), Seq));

   -- Append an element to a sequence
   -- PVS: append(seq: finseq[T], e: T): finseq[T] = seq o singleton_seq(e);
   function Append(Seq : Finseq; E : T) return Finseq is (Concat (Seq, Singleton_Seq (E)));

   -- Remove the element at a specified index
   -- PVS: remove(seq: finseq[T], idx: below(seq`length)): finseq[T] =
   --   (# length := seq`length-1,
   --      seq := LAMBDA(i: below(seq`length-1)):
   --         IF i < idx THEN
   --            seq`seq(i)
   --         ELSE
   --            seq`seq(i+1)
   --         ENDIF #)
   function Remove(Seq : Finseq; Idx : Natural) return Finseq
      with Pre => Idx < Seq.Length;

   -- Count the occurrences of an element in a sequence
   -- PVS: count(seq: finseq[T], e: T): RECURSIVE nat =
   --   IF seq = empty_seq THEN 0
   --   ELSE
   --      (IF car(seq) = e THEN 1 ELSE 0 ENDIF) +
   --      count(cdr(seq), e)
   --   ENDIF
   --   MEASURE seq`length;
   function Count(Seq : Finseq; E : T) return Natural;

   -- Check if an element exists in a sequence
   -- PVS: in?(seq: finseq[T])(e: T): boolean =
   --   EXISTS(i: below(seq`length)):
   --      seq`seq(i) = e;
   function Is_In(Seq : Finseq; E : T) return Boolean is
      (for some I in 0 .. Integer(Seq.Length - 1) => Seq.Seq(I) = E);

   -- Compute the sum of counts of elements in a finite set over a sequence
   -- PVS: sum_of_counts(seq: finseq[T])(s: finite_set[T]): RECURSIVE nat =
   --   IF empty?(s)
   --      THEN 0
   --      ELSE count(seq, choose(s)) + sum_of_counts(seq)(rest(s))
   --   ENDIF
   --   MEASURE card(s);
   function Sum_Of_Counts(Seq : Finseq; S : Fixed_Seq_Array) return Natural;

   -- Reverse the elements of a sequence
   -- PVS: reverse(seq: finseq[T]): finseq[T] =
   --   (# length := seq`length,
   --      seq := LAMBDA (n: below(seq`length)): seq`seq(seq`length - n - 1) #);
   function Reverse_Seq (Seq : Finseq) return Finseq;

   -- Reverse the elements of a sequence (recursively in PVS, iterative in SPARK)
   -- PVS: reverse_rec(seq: finseq[T]): RECURSIVE finseq[T] =
   --   IF seq = empty_seq THEN empty_seq
   --   ELSE append(reverse_rec(cdr(seq)), car(seq))
   --   ENDIF
   --   MEASURE seq`length;
   function Reverse_Rec (Seq : Finseq) return Finseq;


end Finseq_Fns;
