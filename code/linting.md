# Linting

Linting helps everyone agree on a style to use across projects

Are you attached to semi-colons at the end of the line? Linting can help make sure they're always there. 

## Ignores

Sometimes it is necessary to ignore some rules. You're making an informed decision to go against the project's recommendation. Sometimes that is necessary. 

Following summarized via:
https://masteringjs.io/tutorials/eslint/ignore

disable individual lint rules using
 
```
const res = eval('42'); // eslint-disable-line no-eval
```

The `// eslint-disable-line` comment disables the no-eval rule for just that line.

You can also disable the no-eval rule for an entire function block by using /* eslint-disable */.

```
function usesEval() {
  /* eslint-disable no-eval */
  const res = eval('42');
  const res2 = eval('test');

  return res2 + res;
}
```

If you put `/* eslint-disable no-eval */` before any code in a .js file, that will disable the no-eval rule for the entire file.

TODO: Confirm - Only following code block? Or whole file? 

You can also disable all ESLint rules by putting `/* eslint-disable */` at the top of a file.

