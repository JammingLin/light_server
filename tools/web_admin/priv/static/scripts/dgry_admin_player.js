(function defineMustache (global, factory) {
  if (typeof exports === 'object' && exports) {
    factory(exports); // CommonJS
  } else if (typeof define === 'function' && define.amd) {
    define(['exports'], factory); // AMD
  } else {
    factory(global.DGRY_PLAYER = {}); // <script>
  }
}(this, function DGRY_PLAYER_Factory (_me) {
    var ver = '20151105';
    var controller = 'player';
    var players = [];
    var players_page_size = 15;
    var current_player_id = 0;

    _me.init = function(){
        init_render();
        init_events();

        var init_player_id = $('#init_player_id').val();
        if(init_player_id){
            $('#txtSearchKey').val(init_player_id);
            $('input[name=rdoSearchMode][value=player_id]').trigger('click');
        }
        search();
    };

    function init_render(){
        $('#pnl_player_data').hide();
        $('#pnl_player_list').hide();
        $('#pnlInfo').hide();
        $('#btnCancel,#btnSave').hide();
        $('#pnl_player_data div.form-group input').addClass('blank').attr('readonly', 'readonly');
    };

    function init_events(){
        // 切换查询模式
        $('input[name=rdoSearchMode]').click(rdoSearchMode_Click);
        // 回车查询
        $('#txtSearchKey').keydown(txtSearchKey_KeyDown);
        // 按钮查询
        $('#btnSearchPlayer').click(btnSearchPlayer_Click);
        // 修改,保存,取消
        $('#btnModify').click(btnModify_Click);
        $('#btnCancel').click(btnCancel_Click);
        $('#btnSave').click(btnSave_Click);
        $('#lbtnViewCityData').click(lbtnViewCityData_Click);
        $('#lbtnViewWorldData').click(lbtnViewWorldData_Click);
        $('#lbtnViewStatusData').click(lbtnViewStatusData_Click);
        $('#lbtnViewAttackData').click(lbtnViewAttackData_Click);
        $('#lbtnViewCampData').click(lbtnViewCampData_Click);

        $('#btnClearPassport').click(btnClearPassport_Click);
        $('#btnMakeZombie').click(btnMakeZombie_Click);
    };

    /* #region 事件 */
    function txtSearchKey_KeyDown(evt){
        if(evt.keyCode == 13){
            search();
        }
    };

    function rdoSearchMode_Click(){
        var val = $(this).val();
        var phs = {"player_id":"请输入玩家ID",
            "player_account":"请输入完整的玩家账号",
            "player_name":"请输入玩家昵称, 留空则查找全部玩家"
        };
        $('#txtSearchKey').attr('placeholder', phs[val]);
    };

    function btnSearchPlayer_Click(){
        current_player_id = 0;
        $('input[name=page_no]').val(1);
        search();
    };

    function btnModify_Click(){
        if('' == $('input[data-field=player_id]').val()){
            return;
        }
        $(this).hide();
        $('#btnCancel,#btnSave').show();
        $('#pnl_player_data div.editable input').removeClass('blank').removeAttr('readonly');
    };

    function btnCancel_Click(){
        $('#btnModify').show();
        $('#btnCancel,#btnSave').hide();
        var inputs = $('#pnl_player_data div.editable input').addClass('blank').attr('readonly', 'readonly');
        $.each(inputs, function(i,n){
            $(n).val($(n).attr('data-origin-value'));
        });
    };

    function btnSave_Click(){
        save_player();
    };

    function btnClearPassport_Click(){
        if(!confirm('重置角色，此账号会拥有新的玩家ID，变成全新的角色，确定吗？')){
            return;
        }
        DGRY.post(controller, 'clear_passport', {"player_id": current_player_id}, function(rsp){
            if(rsp.code != 1){
                alert(rsp.code);
            }
            else{
                $('#btnClearPassport').hide();
            };
        });
    };

    function btnMakeZombie_Click(){
        DGRY.post(controller, 'make_zombie', {"player_id": current_player_id}, function(rsp){
            if(rsp.code != 1){
                alert(rsp.code);
            }
            else{
                alert(rsp.msg);
            };
        });
    };

    function lbtnViewCityData_Click(){
        get_city_data();
    };

    function lbtnViewWorldData_Click(){
        get_world_data();
    };

    function lbtnViewStatusData_Click(){
        get_status_data();
    };

    function lbtnViewAttackData_Click(){
        get_attack_data();
    };

    function lbtnViewCampData_Click(){
        get_camp_data();
    };

    /* #endregion 事件 */
    function search(){
        var mode = $('input[name=rdoSearchMode]:checked').val();
        var key = $('#txtSearchKey').val();

        if(mode != 'player_name'){
            if(key == ''){
                _alert('请正确输入查询条件.', 'warning');
                return;
            }
            get_player_req(mode, key);
        }
        else{
            var page_no = $('input[name=page_no]').val() || 1;
            get_players_req(mode, key, page_no);
        }
    };

    function get_player_req(mode, key){
        DGRY.post(controller, 'get_player', {"mode":mode, "key": key}, get_player_rsp,
            function(){
                $('#pnl_player_data').hide();
                $('#pnl_player_list').hide();
                $('#pnlInfo').hide();
            });
    };

    function get_player_rsp(rsp){
        if(rsp && rsp.code == 1){
            bind_player_data(rsp.data);
        }
        else{
            _alert('查无结果.' + rsp.msg, 'warning');
        }
    };

    function get_players_req(mode, key, page_no){
        var data = {"mode":mode, "key": key, "page_size": players_page_size, "page_no": page_no};
        DGRY.post(controller, 'get_player', data, get_players_rsp,
            function(){
                $('#pnl_player_data').hide();
                $('#pnl_player_list').hide();
                $('#pnlInfo').hide();
            });
    };

    function get_players_rsp(rsp){
        if(rsp && rsp.code == 1){
            bind_player_list(rsp.data);
        }
        else{
            _alert('查无结果.' + rsp.msg, 'warning');
        }
    };

    function bind_player_list(data){
        var pnl_player_data = $('#pnl_player_data');
        var pnl_player_list = $('#pnl_player_list');
        pnl_player_list.show();
        player_list = {};
        $.each(data.rows, function(i,n){
            player_list[n.player_id] = n;
        });
        var current_page_no = parseInt($('input[name=page_no]').val(), 10) || 1;
        var pager = {"page_no": current_page_no,
        "total":data.total,
        "page_count": Math.floor((data.total + players_page_size - 1) / players_page_size)};

        $.get('/static/html_tplt/player/player_list.mst?_v' + ver, function(template) {
            Mustache.parse(template);   // optional, speeds up future uses
            var rendered = Mustache.render(template, {"items": data.rows, "pager":pager});
            $('#tc_player_list').html(rendered);

            $('.player_item').click(function(){
                $('.player_item').removeClass('selected');
                var player_id = $(this).addClass('selected').attr('data-key');
                bind_player_data(player_list[player_id]);
            });

            bind_pager(pager);

            if(current_player_id){
                $('.player_item[data-key='+current_player_id+']').trigger('click');
            }
            else{
                $('.player_item:first').trigger('click');
            }
        });
    };

    function bind_pager(pager, pagerDom){

            $('.btn-first').click(function(){
                if(pager.page_no <= 1){
                    return;
                }
                current_player_id = 0;
                $('input[name=page_no]').val(1);
                search();
            });
            $('.btn-last').click(function(){
                if(pager.page_no >= pager.page_count){
                    return;
                }
                current_player_id = 0;
                $('input[name=page_no]').val(pager.page_count);
                search();
            });
            $('.btn-prev').click(function(){
                if(pager.page_no <= 1){
                    return;
                }
                current_player_id = 0;
                $('input[name=page_no]').val(pager.page_no - 1);
                search();
            });
            $('.btn-next').click(function(){
                if(pager.page_no >= pager.page_count){
                    return;
                }
                current_player_id = 0;
                $('input[name=page_no]').val(pager.page_no + 1);
                search();
            });
            if(pager.page_no == 1){
                $('.btn-first,.btn-prev').attr('disabled','disabled');
            }
            if(pager.page_no == pager.page_count){
                $('.btn-last,.btn-next').attr('disabled','disabled');
            }

        var options = "";
        for(var i=1; i<= pager.page_count; i++){
            options += '<option value="' + i + '">' + i + '</option>';
        }
        var slt = $('.page_slt').html(options).find('option[value=' + pager.page_no + ']').attr('selected','selected');
        $('.page_slt').change(function(){
                current_player_id = 0;
            $('input[name=page_no]').val($(this).val());
            search();
        });
    };

    function bind_player_data(data){
        var pnl_player_data = $('#pnl_player_data');
        pnl_player_data.show();
        for(var p in data){
            $('[data-field=' + p + ']', pnl_player_data).val(data[p])
            .attr('data-origin-value', data[p]);
        }
        if(!data.account || data.account == ""){
            $('#btnClearPassport').hide();
        }
        else{
            $('#btnClearPassport').show();
        }
        current_player_id = $('input[type=hidden][data-field="player_id"]').val();
    };

    function save_player(){
        var playerData = {};
        $('#pnl_player_data input').each(function(i,n){
            dataField = $(n).attr('data-field');
            if(dataField){
                playerData[dataField] = $(n).val();
            }
        });
        save_player_req(playerData);
    };

    function save_player_req(playerData){
        DGRY.post(controller, 'save_player', playerData, save_player_rsp);
    };

    function save_player_rsp(rsp){
        if(rsp && rsp.code == 1){
            // 重新载入数据
            btnSearchPlayer_Click();
            // 关闭保存按钮
            btnCancel_Click();
            // 提示
            _alert('更新成功.');
        }
        else{
            _alert('更新失败.' + rsp.msg);
        }
    };


    /* 主城数据 */
    function get_city_data(){
        get_city_data_req(current_player_id);
    };

    function get_city_data_req(player_id){
        DGRY.get(controller, 'get_city_data/' + player_id, {}, get_city_data_rsp);
    };

    function get_city_data_rsp(rsp){
        if(rsp && rsp.code == 1){
            bind_city_data(rsp.data);
        }
        else{
            _alert('查无主城数据.' + rsp.msg, 'warning');
        }
    };

    function bind_city_data(data){
        $.get('/static/html_tplt/player/get_city_data.mst?_v' + ver, function(template) {
            Mustache.parse(template);   // optional, speeds up future uses
            var rendered = Mustache.render(template, {"items": data,
                "finish_time_str": function(){
                    var time = DGRY.dgry_time_to_local_time(this.finish_time);
                    return DGRY.format_time(time);
                },
                "finish_time_local_str": function(){
                    var time = DGRY.dgry_time_to_local_time(this.finish_time);
                    return DGRY.format_time(time);
                },
                "time_type_str": function(){
                    var t = this.time_type;
                    return "" + t + ":" + dgry_types_building_time_type[t];
                },
                "building_type": function(){
                    var t = parseInt(this.building_id / 100, 10);
                    return "" + t + ":" + dgry_types_building_types[t];
                }});
            _modal('主城数据', rendered);
        });
    };

    /* 世界数据 */
    function get_world_data(){
        get_world_data_req(current_player_id);
    };

    function get_world_data_req(player_id){
        DGRY.get(controller, 'get_world_data/' + player_id, {}, get_world_data_rsp);
    };

    function get_world_data_rsp(rsp){
        if(rsp && rsp.code == 1){
            bind_world_data(rsp.data);
        }
        else{
            _alert('查无世界数据.' + rsp.msg, 'warning');
        }
    };

    function bind_world_data(data){
        $.get('/static/html_tplt/player/get_world_data.mst?_v' + ver, function(template) {
            Mustache.parse(template);   // optional, speeds up future uses
            var rendered = Mustache.render(template, {"items": data,
                "enemy_type_str": function(){
                    var t = this.enemy_type;
                    return "" + t + ":" + dgry_types_enemy_type[t];
                }});
            _modal('世界数据', rendered);
        });
    };

    /* 状态数据 */
    function get_status_data(){
        get_status_data_req(current_player_id);
    };

    function get_status_data_req(player_id){
        DGRY.get(controller, 'get_status_data/' + player_id, {}, get_status_data_rsp);
    };

    function get_status_data_rsp(rsp){
        if(rsp && rsp.code == 1){
            bind_status_data(rsp.data);
        }
        else{
            _alert('查无状态数据.' + rsp.msg, 'warning');
        }
    };

    function bind_status_data(data){
        $.get('/static/html_tplt/player/get_status_data.mst?_v' + ver, function(template) {
            Mustache.parse(template);   // optional, speeds up future uses
            var rendered = Mustache.render(template,
            {
                "data": data,
                "create_on_str":function(){
                    return DGRY.dgry_time_to_string(this.data.create_on);
                },
                "last_logon_time_str": function(){
                    return DGRY.dgry_time_to_string(this.data.last_logon_time);
                },
                "last_leave_time_str": function(){
                    return DGRY.dgry_time_to_string(this.data.last_leave_time);
                },
                "last_attack_time_str": function(){
                    if(this.data.last_attack_time.year == 1900){
                        return "未攻击过";
                    }
                    return DGRY.dgry_time_to_string(this.data.last_attack_time);
                },
                "last_world_refresh_time_str": function(){
                    if(this.data.last_world_refresh_time.year == 1900){
                        return "未刷新过";
                    }
                    return DGRY.dgry_time_to_string(this.data.last_world_refresh_time);
                }
            });
            _modal('状态数据', rendered);
        });
    };
    /* 攻击数据 */
    function get_attack_data(){};
    /* 兵营数据 */
    function get_camp_data(){};

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
    DGRY_PLAYER.init();
});