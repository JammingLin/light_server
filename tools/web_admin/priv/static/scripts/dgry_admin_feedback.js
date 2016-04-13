(function defineMustache (global, factory) {
  if (typeof exports === 'object' && exports) {
    factory(exports); // CommonJS
  } else if (typeof define === 'function' && define.amd) {
    define(['exports'], factory); // AMD
  } else {
    factory(global.DGRY_FEEDBACK = {}); // <script>
  }
}(this, function DGRY_FEEDBACK_Factory (_me) {
    var controller = 'feedback';
    _me.init = function(){
        init_render();
        init_events();
    };

    function init_render(){

    };

    function init_events(){
        $('#btn_export').click(btn_export_Click);
        $('.btnDelete').click(btnDelete_Click);
    };

    /* #region 事件 */

    function btn_export_Click() {
        export_req();
    };

    function btnDelete_Click(){
        if(confirm('确定删除吗？') == false){
            return;
        }
        var $this = $(this);
        var pid = $this.attr('data-player_id');
        var fid = $this.attr('data-feedback_id');
        delete_req(pid, fid, function(){
            $this.parents('li').hide();
        });
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

    function delete_req(pid, fid, suc_cb){
        DGRY.post(controller, 'delete', {"pid":pid, "fid":fid}, function(rsp){
            delete_rsp(rsp, suc_cb);
        });
    };

    function delete_rsp(rsp, cb){
        if(rsp && rsp.code == 1){
            if(cb){
                cb();
            };
        }
        else{
            _alert('删除错误.' + rsp.msg, 'warning');
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
    DGRY_FEEDBACK.init();
});