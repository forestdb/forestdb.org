// Code boxes

$(function () {
    var code_pres = $("pre").filter(function () {
        var code = $(this).find('code');
        if ($(this).hasClass('norun') || code.hasClass('norun') || code.hasClass('language-norun')) {
            return false;
        };
        return (code.length === 1) && (!$(this.parentNode).is("blockquote"));
    });
    $.each(code_pres,
        function (index, pre) {
            var defaultText = $(pre).find("code").text();
            var EditorModel = require('./editor').EditorModel;
            var editor = new EditorModel(
                {
                    code: defaultText,
                    engine: "webchurch",
                }
            );            
            editor.replaceDomEl(pre);
        });
})
