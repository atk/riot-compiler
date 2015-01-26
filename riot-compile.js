riot.compile = function(tags) {
    tags.replace(/<([a-z\-]+)>([^\x00]*>)((?!<\/?[a-z\-]>)[^\x00]*?)<\/\1>/gi, function(_, tag, html, fn) {
            //    [1         ][2        ][3                        ][4   ]
            // 1: initial tag
            // 2: HTML - everything afterwards (HTML) up to 3 (greedy)
            // 3: JS, non-greedy match for everything without opening or closing tags
            // 4: closing of initial tag

        // make sure IE can use the tag
        document.createElement(tag)

        // register in riot
        var level = 0
        riot.tag(tag, 
            // html attributes without quotes need to be quoted, self-contained attributes "escaped" with __
            html.replace(/(\S+=)(\{.*?\})(\s|>)/g, function(full, attr, val, rest) {
                return (/^(allowfullscreen|async|autofocus|autoplay|checked|compact|controls|declare|default(|checked|muted|selected)|defer|disabled|draggable|enabled|formnovalidate|hidden|indeterminate|inert|ismap|itemscope|loop|multiple|muted|nohref|noresize|noshade|novalidate|nowrap|open|pauseonexit|readonly|required|reversed|scoped|seamless|selected|sortable|spellcheck|translate|truespeed|typemustmatch|visible)=$/i.test(attr) ?
                    '__' : '') + attr + '"' + val + '"' + rest
            }),
            // fn needs to be parsed so the methods can be detected and correctly prefixed/suffixed
            new Function('opts', fn.replace(/\/\*[^\x00]*\*\/|\/\/.*?\n|(["']).*?(?!\\)\1|(?=[\=:;\(\[\{\!][\s\r\n]*)\/.*?\/|(\S+\(.*?\)\s*|)(\{)|(\})/g, function(all, quote, method, open, close) {            
                                        //   [1             ] [2      ] [3              ] [4                               ] [5                 ] [6 ]
                                        // 1: Multiline Comments
                                        // 2: Singleline Comment
                                        // 3: Strings (single/double quote)
                                        // 4: RegExp
                                        // 5: Method start / opening brackets
                                        // 6: closing bracket
                                        // 1-4 are only parsed out so our level count doesn't get influenced by comments, strings or regexp
            return open ?
                level++ == 0 ?
                    method.replace(/(\S+)\s*(\(.*?\))/, 'this.$1 = function$2 {') :
                    method + open :
                close ?
                    (--level == 0 ? '}.bind(this)' : '}') :
                    all
        })))
    })
}