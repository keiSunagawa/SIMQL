import 'brace';
import 'brace/ext/language_tools'

ace.define('ace/mode/simql_highlight_rules', ['require', 'exports', 'module', 'ace/lib/oop', 'ace/lib/lang', 'ace/mode/doc_comment_highlight_rules', 'ace/mode/text_highlight_rules'], function(acequire, exports, module) {
  const oop = acequire('../lib/oop');
  const lang = acequire('../lib/lang');
  const TextHighlightRules = acequire('./text_highlight_rules').TextHighlightRules;

  const SimqlHighlightRules = function() {
    function string(rule) {
      const start = rule.start;
      const escapeSeq = rule.escape;
      return {
        token: 'string.start',
        regex: start,
        next: [
          {token: 'constant.language.escape', regex: escapeSeq},
          {token: 'string.end', next: 'start', regex: start},
          {defaultToken: 'string'}
        ]
      };
    }

    this.$rules = {
      'start': [
        string({start: '"', escape: /\\[0'"bnrtZ\\%_]?/}),
        {
          token: 'keyword',
          regex: '<<|><|<=|>=|==|!=|>|<|:|\\?|@|/>|\\\\>'
        }, {
          token: 'paren.lparen',
          regex: '[\\(]'
        }, {
          token: 'paren.rparen',
          regex: '[\\)]'
        }, {
          token: 'text',
          regex: '\\s+'
        },
        { token: 'variable', regex: '\\$[a-zA-Z][a-zA-Z0-9_\.]*' },
        { token: 'literal.number', regex: '-?(\\d+(\\.\\d*)?|\\d*\\.\\d+)'},
        { token: 'literal.boolean', regex: 'true|false'}
      ],
    };

    this.normalizeRules();
  };

  oop.inherits(SimqlHighlightRules, TextHighlightRules);

  exports.SimqlHighlightRules = SimqlHighlightRules;
});

ace.define('ace/mode/fimql_highlight_rules', ['require', 'exports', 'module', 'ace/lib/oop', 'ace/lib/lang', 'ace/mode/doc_comment_highlight_rules', 'ace/mode/text_highlight_rules'], function(acequire, exports, module) {
  const oop = acequire('../lib/oop');
  const lang = acequire('../lib/lang');
  const TextHighlightRules = acequire('./text_highlight_rules').TextHighlightRules;

  const FimqlHighlightRules = function() {
    function string(rule) {
      const start = rule.start;
      const escapeSeq = rule.escape;
      return {
        token: 'string.start',
        regex: start,
        next: [
          {token: 'constant.language.escape', regex: escapeSeq},
          {token: 'string.end', next: 'start', regex: start},
          {defaultToken: 'string'}
        ]
      };
    }

    const tpe = 'Number|String|Symbol|Boolean|Expr|Raw|List|=>'

    this.$rules = {
      'start': [
        string({start: '"', escape: /\\[0'"bnrtZ\\%_]?/}),
        {
          token: 'def_keyword',
          regex: 'define|defun|let'
        },
        { token: 'tpe', regex: tpe },
        { token: 'generics.start', regex: '<', next: [
          { token: 'tpe', regex: tpe },
          { token: 'tpe', regex: '[A-Z]+'},
          { token: 'generics.end', regex: '>', next: 'start'}
        ] },
        { token: 'query.start', regex: 'q\\{', next: [
          { token: 'string', regex: "[^\\}]+" },
          { token: 'generics.end', regex: '\\}', next: 'start'}
        ] },
        { token: 'variable', regex: '\\$[a-zA-Z][a-zA-Z0-9_\.]*' },
        { token: 'literal.number', regex: '-?(\\d+(\\.\\d*)?|\\d*\\.\\d+)'},
        { token: 'literal.boolean', regex: 'true|false'},
        {
          token: 'paren.lparen',
          regex: '[\\(]'
        }, {
          token: 'paren.rparen',
          regex: '[\\)]'
        }, {
          token: 'text',
          regex: '\\s+'
        }],
    };

    this.normalizeRules();
  };

  oop.inherits(FimqlHighlightRules, TextHighlightRules);

  exports.FimqlHighlightRules = FimqlHighlightRules;
});

