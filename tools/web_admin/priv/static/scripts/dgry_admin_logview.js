(function defineMustache (global, factory) {
  if (typeof exports === 'object' && exports) {
    factory(exports); // CommonJS
  } else if (typeof define === 'function' && define.amd) {
    define(['exports'], factory); // AMD
  } else {
    factory(global.DGRY_LOGVIEW = {}); // <script>
  }
}(this, function DGRY_LOGVIEW_Factory (_me) {
    var ver = '20151110';
    var controller = 'logview';

    _me.init = function(){
        init_render();
        init_events();
    };

    function init_render(){
        var app = $('#sltApps').val();
        load_log_files(app);
    };

    function init_events(){
        $('#sltApps').change(function(){
            load_log_files($(this).val());
        });
    };

    function load_log_files(app) {
        DGRY.get(controller, 'log_files', {"app":app}, load_log_files_rsp);
    };

    function load_log_files_rsp(rsp){
        if(rsp && rsp.code == 1){
            bind_log_files(rsp.data.files);
        }
        else{
            _alert('load_log_files 查无结果.' + rsp.msg, 'warning');
        }
    };

    function bind_log_files(files) {
        var html = [];
        for(var i=0, l = files.length; i < l; i++){
            html.push('<li><a href="javascript:;">' + files[i] + '</a></li>');
        }
        $('#lstLogFiles').html(html.join(''));
        $('#lstLogFiles a').click(function(){
            var app = $('#sltApps').val();
            var file = $(this).text();
            read_log_file(app, file);
        });
    };

    function read_log_file(app, file) {
        $('#log_title').html('<a href="/' +controller + '/read_log_file?app=' + app + '&file=' + file + '" target="_blank">' +
        app + " / " + file + '</a>');

        DGRY.get(controller, 'read_log_file', {"app":app, "file":file}, read_log_file_rsp,
        function(){
            $('#log_content').html('加载中，请稍候！');
        });

    };
    function read_log_file_rsp(rsp) {
        if(rsp && rsp.code == 1){
            if(rsp.data.text == ''){
                $('#log_content').html('无内容');
            }
            else{
                $('#log_content').html(rsp.data.text.replace(/\n/ig, "<br/><br/>"));
            }
        }
        else{
            _alert('read_log_file_rsp 查无结果.' + rsp.msg, 'warning');
        }
    };

    function _modal(title, body, options){
        var m = $('#global_modal');
        $('.modal-title', m).html(title);
        $('.modal-body', m).html(body);
        m.modal(options || {show:false});
        m.modal('show');
    };

    function _alert(msg, alertType){
        var pnlInfo = $('#pnlInfo').attr('class', 'alert alert-dismissible alert-' + (alertType || 'info')).show();
        $('em', pnlInfo).text(msg);
        pnlInfo.fadeOut(5000);
        pnlInfo.unbind('close.bs.alert').on('close.bs.alert', function (e) {
            $(this).hide();
            return false;
        });
    }
}));

$(function(){
    DGRY_LOGVIEW.init();
});