import 'brace';
import 'brace/ext/language_tools'

import './highlight-rule.js'

class CompletionHelper {
  constructor() {
    this.items = [];
  }
}

export const aceCompletionHelper = new CompletionHelper()

ace.define('ace/mode/simql', ['require', 'exports', 'module', 'ace/lib/oop', 'ace/mode/text', 'ace/mode/simql_highlight_rules'],
           function(acequire, exports, module) {
             let oop = acequire('../lib/oop');
             let TextMode = acequire('../mode/text').Mode;
             let SimqlHighlightRules = acequire('./simql_highlight_rules').SimqlHighlightRules;
             // complition
             var langTools = acequire('ace/ext/language_tools');
             var myCompleter = {
               identifierRegexps: [/[^\s]+/],
               getCompletions: function(editor, session, pos, prefix, callback) {
                 callback(
                   null,
                   aceCompletionHelper.items.filter(entry=>{
                     return entry.includes(prefix);
                   }).map(entry=>{
                     return {
                       value: entry
                     };
                   })
                 );
               }
             }
             langTools.addCompleter(myCompleter);

             let Mode = function() {
               this.HighlightRules = SimqlHighlightRules;
               this.$behaviour = this.$defaultBehaviour;
             };
             oop.inherits(Mode, TextMode);

             (function() {
               this.lineCommentStart = ['--', '#']; // todo space
               this.blockComment = {start: '/*', end: '*/'};

               this.$id = 'ace/mode/simql';
             }).call(Mode.prototype);

             exports.Mode = Mode;
           });

ace.define('ace/mode/fimql', ['require', 'exports', 'module', 'ace/lib/oop', 'ace/mode/text', 'ace/mode/fimql_highlight_rules'],
           function(acequire, exports, module) {
             let oop = acequire('../lib/oop');
             let TextMode = acequire('../mode/text').Mode;
             let FimqlHighlightRules = acequire('./fimql_highlight_rules').FimqlHighlightRules;
             // complition
             var langTools = acequire('ace/ext/language_tools');
             const keywords = [
               "definition",
               "defun",
               "let",
               "Number",
               "String",
               "Symbol",
               "Boolean",
               "Expr",
               "Raw",
               "List",
               "true",
               "false"
             ]
             var myCompleter = {
               identifierRegexps: [/[^\s]+/],
               getCompletions: function(editor, session, pos, prefix, callback) {
                 callback(
                   null,
                   keywords.filter(entry=>{
                     return entry.includes(prefix);
                   }).map(entry=>{
                     return {
                       value: entry
                     };
                   })
                 );
               }
             }
             langTools.addCompleter(myCompleter);

             let Mode = function() {
               this.HighlightRules = FimqlHighlightRules;
               this.$behaviour = this.$defaultBehaviour;
             };
             oop.inherits(Mode, TextMode);

             (function() {
               this.lineCommentStart = ['--', '#']; // todo space
               this.blockComment = {start: '/*', end: '*/'};

               this.$id = 'ace/mode/fimql';
             }).call(Mode.prototype);

             exports.Mode = Mode;
           });
