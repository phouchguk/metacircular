/* global console, process, require */

var fs = require("fs");
var readline = require("readline");

// TOKEN READER

function Rdr(ix) {
  this.len = ix.length;
  this.cursor = 0;
  this.ix = ix;
}

Rdr.prototype.peek = function() {
  if (this.cursor === this.len) {
    return null;
  }

  return this.ix[this.cursor];
};

Rdr.prototype.pop = function() {
  if (this.cursor === this.len) {
    return null;
  }

  return this.ix[this.cursor++];
};

// T OKENISE

function TokenResult(strs, tokens) {
  this.strs = strs;
  this.tokens = new Rdr(tokens);
}

var tokenise = function(s) {
  var strs = [];

  var reComment = /;(.*)\n/g;
  var reStr = /"(?:[^"\\]|\\.)*"/g;
  var reDelimiter = /\(|\)|\[|\]|'|`|,/g;
  var reDupeSpace = /\s\s+/g;

  var replaceStrs = function(m) {
    var i = strs.length;
    strs[i] = m.substring(1, m.length - 1);
    return "$" + i;
  };

  var addSpaces = function(m) {
    if (m === "[") {
      m = "(";
    }

    if (m === ")") {
      m = ")";
    }

    return " " + m + " ";
  };

  return new TokenResult(
    strs,
    s
      .replace(reStr, replaceStrs)
      .replace(reComment, "\n")
      .replace(reDelimiter, addSpaces)
      .replace(reDupeSpace, " ")
      .trim()
      .split(" ")
  );
};

// nil

var nil = "nil";

// PAIR

var numberP, pairP;

var validTypeP = function(x) {
  return (
    typeof x === "function" ||
    typeof x === "string" ||
    numberP(x) ||
    pairP(x) ||
    (typeof x === "object" && typeof x._parent !== "undefined")
  );
};

function Pair(car, cdr) {
  if (!validTypeP(car)) {
    throw new Error("car is not a valid type - " + car);
  }

  if (!validTypeP(cdr)) {
    throw new Error("cdr is not a valid type - " + cdr);
  }

  this.car = car;
  this.cdr = cdr;
}

var car = function(x) {
  if (!pairP(x)) {
    throw new Error("can't take car of non-pair - " + x);
  }

  return x.car;
};

var cdr = function(x) {
  if (!pairP(x)) {
    throw new Error("can't take cdr of non-pair - " + x);
  }

  return x.cdr;
};

var cons = function(car, cdr) {
  return new Pair(car, cdr);
};

var list = function() {
  var l = nil;
  var a = [].slice.call(arguments);

  for (var i = a.length - 1; i >= 0; i--) {
    l = cons(a[i], l);
  }

  return l;
};

var listP = function(x) {
  return x === nil || pairP(x);
};

var listToArray = function(xs) {
  var arr = [];

  while (xs !== nil) {
    arr.push(car(xs));
    xs = cdr(xs);
  }

  return arr;
};

pairP = function(x) {
  return x instanceof Pair;
};

var print;

var setCar = function(p, val) {
  if (!pairP(p)) {
    throw new Error(
      "can't take set-car! of non-pair - " + p + " to " + print(val, true)
    );
  }

  p.car = val;
  return "ok";
};

var setCdr = function(p, val) {
  if (!pairP(p)) {
    throw new Error(
      "can't take set-cdr! of non-pair - " + p + " to " + print(val, true)
    );
  }

  p.cdr = val;
  return "ok";
};

// PARSE

var parse;

var parseList = function(tr) {
  var exp, last, list;

  list = last = nil;
  while (tr.tokens.peek() !== null) {
    if (tr.tokens.peek() === ")") {
      tr.tokens.pop(); // remove trailing ')'
      return list;
    }

    if (tr.tokens.peek() === ".") {
      tr.tokens.pop();
      last.cdr = parse(tr);

      // remove trailing ')'
      tr.tokens.pop();

      return list;
    }

    exp = parse(tr);

    if (last === nil) {
      list = cons(exp, nil);
      last = list;
    } else {
      last.cdr = cons(exp, nil);
      last = cdr(last);
    }
  }

  throw new Error("expected ')'");
};

parse = function(tr) {
  var token = tr.tokens.pop();

  if (token === "(") {
    return parseList(tr);
  }

  if (token === "'") {
    return cons("quote", cons(parse(tr), nil));
  }

  // string?
  if (token[0] === "$") {
    return "$" + tr.strs[parseInt(token.substring(1), 10)];
  }

  var nr = parseFloat(token);

  if (isNaN(nr)) {
    // symbol
    return token;
  }

  // number
  return nr;
};

// conversion

var jsToStr = function(s) {
  return "$" + s;
};

var numToStr = function(n) {
  return jsToStr(n + "");
};

var strToJs = function(s) {
  return s.substring(1);
};

var strToSym = strToJs;

var strToNum = function(s) {
  var nr = parseFloat(strToJs(s));

  return isNaN(nr) ? nil : nr;
};

var symToStr = jsToStr;

// PRINT

print = function(e, readably) {
  if (typeof e === "number") {
    return e;
  }

  if (typeof e === "string") {
    if (e[0] === "$") {
      e = e.substring(1);
      // string
      if (readably) {
        return (
          '"' +
          e
            .replace(/\\/g, "\\")
            .replace(/\r/g, "\\r")
            .replace(/\n/g, "\\n")
            .replace(/\t/g, "\\t") +
          '"'
        );
      } else {
        return e;
      }
    } else {
      // symbol
      return e;
    }
  }

  if (pairP(e)) {
    var res = "(";

    while (e !== nil) {
      res += print(car(e)) + " ";

      if (!listP(cdr(e))) {
        res += ". " + print(cdr(e), true) + " ";
        break;
      }

      e = cdr(e);
    }

    return res.substring(0, res.length - 1) + ")";
  }

  if (typeof e === "object" && typeof e._parent !== "undefined") {
    return "<env>";
  }

  throw new Error("don't know how to print: " + e);
};

// EVAL - APPLY

// helpers

var applicationP = pairP;

var primitiveImplementation;

var applyPrimitiveProcedure = function(f, args) {
  return primitiveImplementation(f).apply(null, listToArray(args));
};

var taggedListP;

var assignmentP = function(p) {
  return taggedListP(p, "set!");
};

var assignmentValue = function(exp) {
  return car(cdr(cdr(exp)));
};

var assignmentVariable = function(exp) {
  return car(cdr(exp));
};

var beginActions = function(exp) {
  return cdr(exp);
};

var beginP = function(p) {
  return taggedListP(p, "begin");
};

var compoundProcedure = function(p) {
  return taggedListP(p, "procedure");
};

var condActions = cdr;

var condClauses = cdr;

var condPredicate = car;

var condElseClause = function(clause) {
  return condPredicate(clause) === "else";
};

var condP = function(p) {
  return taggedListP(p, "cond");
};

var expandClauses;

var condToIf = function(exp) {
  return expandClauses(condClauses(exp));
};

var findVariableEnvironment, findRoot;

var defineVariable = function(key, val, env) {
  var found = findVariableEnvironment(key, env);

  if (found === null) {
    var root = findRoot(env);
    root[key] = val;
  } else {
    found[key] = val;
  }

  return "ok";
};

var definitionP = function(p) {
  return taggedListP(p, "define");
};

var makeLambda, symbolP;

var definitionValue = function(exp) {
  if (symbolP(car(cdr(exp)))) {
    return car(cdr(cdr(exp)));
  } else {
    return makeLambda(cdr(car(cdr(exp))), cdr(cdr(exp)));
  }
};

var definitionVariable = function(exp) {
  return symbolP(car(cdr(exp))) ? car(cdr(exp)) : car(car(cdr(exp)));
};

var evl, setVariableValue;

var evalAssignment = function(exp, env) {
  setVariableValue(
    assignmentVariable(exp),
    evl(assignmentValue(exp), env),
    env
  );

  return "ok";
};

var evalDefinition = function(exp, env) {
  defineVariable(definitionVariable(exp), evl(definitionValue(exp), env), env);

  return "ok";
};

var firstExp, lastExpP, restExp;

var evalSequence = function(exps, env) {
  while (exps !== nil) {
    if (lastExpP(exps)) {
      return firstExp(exps);
    }

    evl(firstExp(exps), env);

    exps = restExp(exps);
  }

  return nil;
};

var makeIf, nullP, sequenceToExp;

expandClauses = function(clauses) {
  if (nullP(clauses)) {
    return false;
  }

  var first = car(clauses);
  var rest = cdr(clauses);

  if (condElseClause(first)) {
    if (nullP(rest)) {
      return sequenceToExp(condActions(first));
    }

    throw new Error("ELSE clause isn't last -- cond->if");
  }

  return makeIf(
    condPredicate(first),
    sequenceToExp(condActions(first)),
    expandClauses(rest)
  );
};

var length;

var extendEnvironment = function(vars, vals, baseEnv) {
  var env = { _parent: baseEnv };

  if (length(vars) === length(vals)) {
    while (vars !== nil) {
      env[car(vars)] = car(vals);

      vars = cdr(vars);
      vals = cdr(vals);
    }

    return env;
  } else {
    if (length(vars) < length(vals)) {
      throw new Error("Too many arguments supplied");
    } else {
      throw new Error("Too few arguments supplied");
    }
  }
};

var falseP = function(exp) {
  return exp === "false";
};

findRoot = function(env) {
  while (env._parent !== nil) {
    env = env._parent;
  }

  return env;
};

findVariableEnvironment = function(key, env) {
  while (env !== nil) {
    if (env.hasOwnProperty(key)) {
      return env;
    }

    env = env._parent;
  }

  return null;
};

firstExp = car;

var firstOperand = car;

var ifAlternative = function(exp) {
  if (!nullP(cdr(cdr(cdr(exp))))) {
    return car(cdr(cdr(cdr(exp))));
  }

  return "false";
};

var ifConsequent = function(exp) {
  return car(cdr(cdr(exp)));
};

var ifP = function(p) {
  return taggedListP(p, "if");
};

var ifPredicate = function(exp) {
  return car(cdr(exp));
};

var lambdaBody = function(exp) {
  return cdr(cdr(exp));
};

var lambdaP = function(p) {
  return taggedListP(p, "lambda");
};

var lambdaParameters = function(exp) {
  return car(cdr(exp));
};

lastExpP = function(seq) {
  return nullP(cdr(seq));
};

length = function(seq) {
  var i = 0;

  while (seq !== nil) {
    i++;
    seq = cdr(seq);
  }

  return i;
};

var letBody = function(exp) {
  return cdr(cdr(exp));
};

var letP = function(p) {
  return taggedListP(p, "let");
};

var letValues, letVariables, makeLet;

var letToLambda = function(exp) {
  return makeLet(letVariables(exp), letValues(exp), letBody(exp));
};

var mapr;

letValues = function(exp) {
  return mapr(function(x) {
    return car(cdr(x));
  }, car(cdr(exp)));
};

letVariables = function(exp) {
  return mapr(car, car(cdr(exp)));
};

var noOperandsP, restOperands;

var listOfValues = function(exps, env) {
  if (noOperandsP(exps)) {
    return nil;
  }

  return cons(
    evl(firstOperand(exps), env),
    listOfValues(restOperands(exps), env)
  );
};

var lookupVariableValue = function(key, env) {
  var found = findVariableEnvironment(key, env);

  if (found === null) {
    throw "unbound variable - " + key;
  }

  return found[key];
};

var makeBegin = function(seq) {
  return cons("begin", seq);
};

makeIf = function(predicate, consequent, alternative) {
  return cons("if", cons(predicate, cons(consequent, cons(alternative, nil))));
};

makeLambda = function(parameters, body) {
  return cons("lambda", cons(parameters, body));
};

makeLet = function(parameters, arguments, body) {
  return cons(makeLambda(parameters, body), arguments);
};

var makeProcedure = function(parameters, body, env) {
  return cons("procedure", cons(parameters, cons(body, cons(env, nil))));
};

nullP = function(exp) {
  return exp === nil;
};

noOperandsP = nullP;

var operator = car;

var operands = cdr;

primitiveImplementation = function(proc) {
  return car(cdr(proc));
};

var primitiveProcedureP = function(p) {
  return taggedListP(p, "primitive");
};

var procedureBody = function(p) {
  return car(cdr(cdr(p)));
};

var procedureEnvironment = function(p) {
  return car(cdr(cdr(cdr(p))));
};

var procedureParameters = function(p) {
  return car(cdr(p));
};

var quotedP = function(p) {
  return taggedListP(p, "quote");
};

restExp = cdr;

restOperands = cdr;

sequenceToExp = function(seq) {
  if (nullP(seq)) {
    return nil;
  }

  if (lastExpP(seq)) {
    return firstExp(seq);
  }

  return makeBegin(seq);
};

setVariableValue = function(key, value, env) {
  var found = findVariableEnvironment(key, env);

  if (found === null) {
    throw "unbound variable - " + key;
  }

  return (found[key] = value);
};

symbolP = function(exp) {
  return typeof exp === "string" && exp[0] !== "$";
};

var selfEvaluatingP = function(exp) {
  if (typeof exp === "number") {
    return true;
  }

  if (typeof exp === "string" && exp[0] === "$") {
    return true;
  }

  return false;
};


taggedListP = function(exp, tag) {
  return listP(exp) && car(exp) === tag;
};

var textOfQuotation = function(exp) {
  return car(cdr(exp));
};

var time = function() {
  return new Date().getTime();
};

var trueP = function(exp) {
  return !falseP(exp);
};

var variableP = symbolP;

// eval/apply

var ap;

evl = function(exp, env) {
  while (true) {
    if (selfEvaluatingP(exp)) {
      return exp;
    }

    if (variableP(exp)) {
      return lookupVariableValue(exp, env);
    }

    if (quotedP(exp)) {
      return textOfQuotation(exp);
    }

    if (assignmentP(exp)) {
      return evalAssignment(exp, env);
    }

    if (definitionP(exp)) {
      return evalDefinition(exp, env);
    }

    if (ifP(exp)) {
      if (trueP(evl(ifPredicate(exp), env))) {
        exp = ifConsequent(exp);
      } else {
        exp = ifAlternative(exp);
      }

      continue;
    }

    if (lambdaP(exp)) {
      return makeProcedure(lambdaParameters(exp), lambdaBody(exp), env);
    }

    if (beginP(exp)) {
      exp = evalSequence(beginActions(exp), env);
      continue;
    }

    if (condP(exp)) {
      exp = condToIf(exp);
      continue;
    }

    if (letP(exp)) {
      exp = letToLambda(exp);
      continue;
    }

    if (applicationP(exp)) {
      var res = ap(evl(operator(exp), env), listOfValues(operands(exp), env));

      if (res.primitive) {
        return res.primitive;
      }

      env = res.env;
      exp = res.exp;
      continue;
    }

    console.log(print(exp, true));
    throw new Error("Unknown expression type -- EVAL");
  }
};

ap = function(procedure, arguments) {
  if (primitiveProcedureP(procedure)) {
    return { primitive: applyPrimitiveProcedure(procedure, arguments) };
  }

  if (!compoundProcedure(procedure)) {
    throw new Error("Unknown procedure type -- APPLY");
  }

  var env = extendEnvironment(
    procedureParameters(procedure),
    arguments,
    procedureEnvironment(procedure)
  );

  var exp = evalSequence(procedureBody(procedure), env);

  return { env: env, exp: exp };
};

// BIF

var boolify = function(f) {
  return function() {
    var args = [].slice.call(arguments);
    return f.apply(null, args) ? "true" : "false";
  };
};

var display = function(x) {
  console.log(print(x, true));
  return "nil";
};

var error = function() {
  var args = [].slice.call(arguments);
  var len = args.length;

  if (len > 1) {
    for (var i = 1; i < len; i++) {
      console.log(print(args[i], true));
    }
  }

  throw new Error(print(args[0], true));
};

var myApply = function(f, args) {
  var result = ap(f, args);

  if (result.primitive) {
    return result.primitive;
  }

  return evl(result.exp, result.env);
}

var add = function(a, b) {
  return a + b;
};

var div = function(a, b) {
  return a / b;
};

var eq = function(a, b) {
  return a === b;
};

var gt = function(a, b) {
  return a > b;
};

var lt = function(a, b) {
  return a < b;
};

var lispDepth = 0;

var mul = function(a, b) {
  return a * b;
};

numberP = function(x) {
  return typeof x === "number";
};

var read = function(s) {
  return parse(tokenise(strToJs(s)));
};

var incLispDepth = function() {
  lispDepth++;
  rl._prompt = getPrompt();
  return "ok";
};

var slurp = function(name) {
  return jsToStr(fs.readFileSync(strToJs(name)));
};

var stringP = function(x) {
  return typeof x === "string" && x[0] === "$";
};

var sub = function(a, b) {
  return a - b;
};

var symbolP = function(x) {
  return typeof x === "string" && x[0] !== "$";
};

// RUN

mapr = function(f, xs) {
  var l = nil;

  while (xs !== nil) {
    l = cons(f(car(xs)), l);
    xs = cdr(xs);
  }

  return l;
};

var primitiveProcedures = list(
  list("+", add),
  list("/", div),
  list("*", mul),
  list("-", sub),
  list("=", boolify(eq)),
  list(">", boolify(gt)),
  list("<", boolify(lt)),
  list("apply", myApply),
  list("car", car),
  list("cdr", cdr),
  list("cons", cons),
  list("display", display),
  list("eq?", boolify(eq)),
  list("error", error),
  list("eval", evl),
  list("inc-lisp-depth!", incLispDepth),
  list("list", list),
  list("not", boolify(falseP)),
  list("null?", boolify(nullP)),
  list("number?", boolify(numberP)),
  list("number->string", numToStr),
  list("pair?", boolify(pairP)),
  list("read", read),
  list("set-car!", setCar),
  list("set-cdr!", setCdr),
  list("inc-lisp-depth!", incLispDepth),
  list("slurp", slurp),
  list("string?", boolify(stringP)),
  list("string->symbol", strToSym),
  list("string->number", strToNum),
  list("symbol?", boolify(symbolP)),
  list("symbol->string", symToStr),
  list("time", time)
);

var primitiveProcedureNames = function() {
  return mapr(car, primitiveProcedures);
};

var primitiveProcedureObjects = function() {
  return mapr(function(proc) {
    return list("primitive", car(cdr(proc)));
  }, primitiveProcedures);
};

var theEmptyEnvironment = nil;

var setupEnvironment = function() {
  var initialEnv = extendEnvironment(
    primitiveProcedureNames(),
    primitiveProcedureObjects(),
    theEmptyEnvironment
  );

  defineVariable("true", "true", initialEnv);
  defineVariable("false", "false", initialEnv);

  return initialEnv;
};

var theGlobalEnvironment = setupEnvironment();
defineVariable("the-global-environment", theGlobalEnvironment, theGlobalEnvironment);

/*
console.log(
  print(
    evl(
      parse(tokenise("(begin " + fs.readFileSync("lisp.scm") + ")")),
      theGlobalEnvironment
    ),
    true
  )
);
*/

var getPrompt = function() {
  return "lisp" + lispDepth + "> ";
};

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: getPrompt()
});

rl.prompt();

rl.on("line", function(line) {
  line = line.trim();
  switch (line) {
    case "(quit)":
      process.exit(0);
      break;
    default:
      //var line = "(eval '" + line + " the-empty-environment)";
      //console.log(line);
      console.log(
        print(evl(parse(tokenise(line)), theGlobalEnvironment), true)
      );
      break;
  }
  rl.prompt();
}).on("close", function() {
  process.exit(0);
});

//console.log(print(evl(parse(tokenise(testStr)), core), true));
