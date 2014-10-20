(* imports *)
val toString = Int.toString
val length = List.length
val filter = List.filter
val app = List.app

(* Unit testing library for Standard ML. Motivations here are:
 * 1: When a test fails, I want a description of how it failed. The problem
 *    here is there's no way to convert an arbitrary value to a string. However,
 *    the interpreter can display arbitrary values, so we have tests return
 *    a record which contains the detail, and if we bind it to a variable,
 *    we can see the result in our list of bindings. It's dirty, but it works.
 * 2: When we have a lot of tests, we don't want to scan the entire list of
 *    bindings to work out where we stand, so it should keep track of our 
 *    successes and failures so we can see a summary after we've run all our
 *    tests. This can't contain the reasons, because we can neither convert them
 *    to strings or retain them raw, as that would result in heterogenous lists
 *    which we don't support: so this is limited to test names and PASS/FAIL.
 * 3: It's also nice to be able to express this concisely, with sensible names 
 *    (eg allowing symbols, spaces), and to be able to disable tests during 
 *    development, so there's a bit of sugar here too.
 * 4: Whilst I haven't provided anything other than a nice equality checking
 *    matcher yet, the syntax is sufficiently versatile to allow things like
 *    Hamcrest matchers, and gives potential to easily implement custom 
 *    descriptions. Examples will follow when I have reason to write them :) *)

datatype 'a result = PASS  | FAIL of {expected: 'a, actual: 'a}
datatype IGNORED = IGNORED
val results: {name: string, result: string} list ref = ref []

fun describe name PASS = {name=name, result="Passed"}
  | describe name (FAIL _) = {name=name, result="Failed"}

fun record name result = results := (describe name result) :: !results

fun test name result = (record name result; {Test = name, Result = result})
fun ignore test name result = {Test = name, Result = IGNORED}

fun status expectation {name = _, result = result} = result = expectation 

fun reset() = results := []
fun summarise() = 
  let fun print_summary {name=name, result=result} = print(result ^ ": " ^ name ^ "\n")
      val results = !results
      val passed = toString(length (filter (status "Passed") results ))
      val failed = toString(length (filter (status "Failed") results )) 
      val totals = "Totals: " ^ passed ^ " passed, " ^ failed ^ " failed.\n"
  in (app print_summary (filter (status "Failed") results ); print totals) 
  end

infix ==
fun op ==(actual, expected) = 
   if actual = expected
   then PASS
   else FAIL {expected=expected, actual=actual}

infix raises
fun op raises((function, arguments), exc) = 
   let val expected = exnName exc
   in (function arguments; FAIL {expected = expected ^ " to be raised", actual = "No exception raised"}) 
      handle e => let val raised = exnName e
                  in if raised = expected 
                     then PASS
                     else FAIL {expected = expected ^ " to be raised", actual = raised ^ " raised" }
                  end
   end