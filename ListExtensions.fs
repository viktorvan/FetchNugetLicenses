namespace Extensions    
    module Result =
        let apply f x =
            match f,x with
            | Ok f, Ok x -> Ok (f x)
            | Error f, _ -> Error f
            | _, Error x -> Error x

    module Async =
        let map f xAsync = async {
            // get the contents of xAsync 
            let! x = xAsync 
            // apply the function and lift the result
            return f x
            }

        let retn x = async {
            // lift x to an Async
            return x
            }

        let apply fAsync xAsync = async {
            // start the two asyncs in parallel
            let! fChild = Async.StartChild fAsync
            let! xChild = Async.StartChild xAsync

            // wait for the results
            let! f = fChild
            let! x = xChild 

            // apply the function to the results
            return f x 
            }

        let bind f xAsync = async {
            // get the contents of xAsync 
            let! x = xAsync 
            // apply the function but don't lift the result
            // as f will return an Async
            return! f x
            }

    module List =

        /// Map a Async producing function over a list to get a new Async 
        /// using applicative style
        /// ('a -> Async<'b>) -> 'a list -> Async<'b list>
        let rec traverseAsyncA f list =

            // define the applicative functions
            let (<*>) = Async.apply
            let retn = Async.retn

            // define a "cons" function
            let cons head tail = head :: tail

            // right fold over the list
            let initState = retn []
            let folder head tail = 
                retn cons <*> (f head) <*> tail

            List.foldBack folder list initState 

        /// Transform a "list<Async>" into a "Async<list>" 
        /// and collect the results using apply.
        let sequenceAsyncA x = traverseAsyncA id x

        /// Map a Result producing function over a list to get a new Result 
        /// using applicative style
        /// ('a -> Result<'b>) -> 'a list -> Result<'b list>
        let rec traverseResultA f list =

            // define the applicative functions
            let (<*>) = Result.apply
            let retn = Result.Ok

            // define a "cons" function
            let cons head tail = head :: tail

            // right fold over the list
            let initState = retn []
            let folder head tail = 
                retn cons <*> (f head) <*> tail

            List.foldBack folder list initState 

        /// Transform a "list<Result>" into a "Result<list>" 
        /// and collect the results using apply.
        let sequenceResultA x = traverseResultA id x