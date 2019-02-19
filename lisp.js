/* global console */

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
      .replace(reDelimiter, addSpaces)
      .replace(reDupeSpace, " ")
      .trim()
      .split(" ")
  );
};

// PAIR

function Pair(car, cdr) {
  this.car = car;
  this.cdr = cdr;
}

var nil = "nil";

var car = function(x) {
  return x.car;
};

var cdr = function(x) {
  return x.cdr;
};

var cons = function(car, cdr) {
  return new Pair(car, cdr);
};

var pairP;

var listP = function(x) {
  return x === nil || pairP(x);
};

var listToArray = function(xs) {
  var arr = [];

  while (xs !== nil) {
    arr.push(xs.car);
    xs = xs.cdr;
  }

  return arr;
};

pairP = function(x) {
  return x instanceof Pair;
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
      last = last.cdr;
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

// PRINT

var print = function(e, readably) {
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
      res += print(e.car) + " ";

      if (!listP(e.cdr)) {
        res += ". " + print(e.cdr, true) + " ";
        break;
      }

      e = e.cdr;
    }

    return res.substring(0, res.length - 1) + ")";
  }
};

// EVAL - APPLY

// helpers

var applicationP = pairP;

var applyPrimitiveProcedure = function(f, args) {
  return f.apply(null, listToArray(args));
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
  definitionVariable(
    definitionVariable(exp),
    evl(definitionValue(exp), env),
    env
  );

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
      env[vars.car] = vals.car;

      vars = vars.cdr;
      vals = vals.cdr;
    }
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

var findVariableValue = function(key, env) {
  while (env !== null) {
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
    seq = seq.cdr;
  }

  return i;
};

var noOperandsP, restOperands;

var listOfValues = function(exps, env) {
  if (noOperandsP(exps)) {
    return nil;
  }

  cons(evl(firstOperand(exps), env), listOfValues(restOperands(exps), env));
};

var lookupVariableValue = function(key, env) {
  var found = findVariableValue(key, env);

  if (found === null) {
    throw "unbound variable - " + key;
  }

  return found[key];
};

var makeBegin = function(seq) {
  cons("begin", seq);
};

makeIf = function(predicate, consequent, alternative) {
  cons("if", cons(predicate, cons(consequent, cons(alternative, nil))));
};

makeLambda = function(parameters, body) {
  cons("lambda", cons(parameters, body));
};

var makeProcedure = function(parameters, body, env) {
  cons("procedure", cons(parameters, cons(body, cons(env, nil))));
};

noOperandsP = nullP;

nullP = function(exp) {
  return exp === nil;
};

var operator = car;

var operands = cdr;

var primitiveProcedureP = function(exp) {
  return typeof exp === "function";
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
  var found = findVariableValue(key, env);

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
      exp = lookupVariableValue(exp, env);
      continue;
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
      env = condToIf(exp);
      continue;
    }

    if (applicationP(exp)) {
      var res = ap(evl(operator(exp), env), listOfValues(operands(exp), env));
      env = res.env;
      exp = res.exp;
    }

    throw new Error("Unknown expression type -- EVAL");
  }
};

ap = function(procedure, arguments) {
  if (primitiveProcedureP(procedure)) {
    return applyPrimitiveProcedure(procedure, arguments);
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

var testStr = "'a";
var core = { _parent: null };

console.log(print(evl(parse(tokenise(testStr)), core), true));
