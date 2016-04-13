(function defineMustache (global, factory) {
  if (typeof exports === 'object' && exports) {
    factory(exports); // CommonJS
  } else if (typeof define === 'function' && define.amd) {
    define(['exports'], factory); // AMD
  } else {
    factory(global.DGRY_WORLD_CHAT = {}); // <script>
  }
}(this, function DGRY_WORLD_CHAT_Factory (_me) {
    var controller = 'world_chat';
    _me.init = function(){
        init_render();
        init_events();
    };

    function init_render(){

    };

    function init_events(){
        $('#btn_export').click(btn_export_Click);
    };

    /* #region 事件 */

    function btn_export_Click() {
        export_req();
    };

    /* #endregion 事件 */

    function export_req(){
        DGRY.get(controller, 'export', {}, export_rsp);
    };

    function export_rsp(rsp){
        if(rsp && rsp.code == 1){
            window.open(rsp.data.file);
        }
        else{
            _alert('导出错误.' + rsp.msg, 'warning');
        }
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
    DGRY_WORLD_CHAT.init();
});