  type +'a t
  (** ['a t] represents a blocking monad state *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [a >>= b] will pass the result of [a] to the
      [b] function.  This is a monadic [bind]. *)

  val return : 'a -> 'a t
  (** [return a] will construct a constant IO value. *)

  type ic
  (** [ic] represents an input channel *)

  type oc
  (** [oc] represents an output channel *)

  val read_line : ic -> string option t
  (** [read_line ic] will read a single line terminated
      by CR or CRLF from the input channel [ic].  It returns
      {!None} if EOF or other error condition is reached. *)

  val read : ic -> int -> string t
  (** [read ic len] will block until a maximum of [len] characters
      are read from the input channel [ic].  It returns an
      empty string if EOF or some other error condition occurs
      on the input channel, and can also return fewer than [len]
      characters if input buffering is not sufficient to satisfy the
      request. *)

  val write : oc -> string -> unit t
  (** [write oc s] will block until the complete [s] string is
      written to the output channel [oc]. *)

  val flush : oc -> unit t
  (** [flush oc] will return when all previously buffered content
      from calling {!write} have been written to the output channel
      [oc]. *)
