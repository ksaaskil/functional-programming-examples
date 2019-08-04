---
title: How I got to functional programming
published: false
description: How I learned to love immutability
tags: scala, typescript fp
# canonical_url: link for the canonical version of the content
# cover_image: cover image for post, accepts a URL. 
The best size is 1000 x 420.
# series: post series name.
---

## How and why I learned functional programming

Senior dev recommended (1) avoiding mutations and (2) chaining operations.

### Go immutable when possible

- University: I had taken courses in Java and C and been writing C++ programs doing stuff like `simulationRunner.update(...)`
- My first programs at a real job where of the kind `solver = new MyAlgorithm(); solver.addData(requiredData); result = solver.solve(input)`
- "My algorithm instance needs some data, I'll use a setter for that!"
- One does not need functional programming to understand this was silly: The algorithm had no reason to maintain internal state so a better solution would be `solver = new MyAlgorithm(requiredData); result = solver.solve(input)`
- If you find yourself doing stuff like `result = new Algorithm().solve(input)`, think carefully if `Algorithm` needs to be a class. Maybe it could be a static class?
- Lesson: Use immutable objects and data structures when possible
- Lesson: Not everything needs to be a class

### For-loops are not the only way to iterate

- Lesson: methods/functions such as map, reduce, and filter not only shorten the code but can make it a lot easier to read
- Functions such as map are not only for arrays

### There are alternatives to if-else and try-catch (Option, Either)

- Our Java codebase had lots of `if (input == null) {} else {}` types of checks, signifying that some functions were prepared to handle null inputs so that users would not need to worry about that
- For example, `user.email` could be null if the user had not given their email address so every operation using it needed `if (user.email !== null) { sendEmailToUser(user.email, content) }`
- But whose responsibility is it to handle the null and should every function do that?
- One part of the algorithm used Spark so we used Scala
- In Scala, many built-in functions return types such as `Option` where old-school Java would return `null`
- `Option` is null-safe, ensuring that the user handles the `Empty` case properly

<!--
### Side effects are annoying to test
-->

