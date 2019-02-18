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
    strs[i] = m;
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

var cons = function(car, cdr) {
  return new Pair(car, cdr);
};

var isList = function(x) {
  return x === nil || x instanceof Pair;
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

  // string?
  if (token[0] === "$") {
    return tr.strs[parseInt(token.substring(1), 10)];
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

  if (e instanceof Pair) {
    var res = "(";

    while (e !== nil) {
      res += print(e.car) + " ";

      if (!isList(e.cdr)) {
        res += ". " + print(e.cdr, true) + " ";
        break;
      }

      e = e.cdr;
    }

    return res.substring(0, res.length - 1) + ")";
  }
};

var testStr = '(1 "hello" 3 (1 . 2))';

console.log(print(parse(tokenise(testStr)), true));
