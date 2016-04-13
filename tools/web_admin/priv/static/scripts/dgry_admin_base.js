(function defineMustache (global, factory) {
  if (typeof exports === 'object' && exports) {
    factory(exports); // CommonJS
  } else if (typeof define === 'function' && define.amd) {
    define(['exports'], factory); // AMD
  } else {
    factory(global.DGRY_BASE = {}); // <script>
  }
}(this, function DGRY_BASE_Factory (_me) {
    _me.data = {account:'', server:''},
    _me.init = function(){
        init_render();
        init_events();
    };

    function init_render(){
        $('#nav-item-logined').hide();
        load_data_req();
    };

    function init_events(){
    };

    function load_data_req(){
        DGRY.get('account', 'info', null, load_data_rsp, null);
    };

    function load_data_rsp(rsp){
        if(rsp.code == 1){
            _me.data.account = rsp.data.account;
            $('#nav-item-unlogin').hide();
            $('#nav-item-logined').show();
            $('#nav-item-logined em').text(_me.data.account);
        }
        else{
            $('#nav-item-unlogin').show();
            $('#nav-item-logined').hide();
        }
    };
}));

$(function(){
    DGRY_BASE.init();
});