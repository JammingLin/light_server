(function defineMustache (global, factory) {
  if (typeof exports === 'object' && exports) {
    factory(exports); // CommonJS
  } else if (typeof define === 'function' && define.amd) {
    define(['exports'], factory); // AMD
  } else {
    factory(global.DGRY = {}); // <script>
  }
}(this, function dgryFactory (_me) {
    _me.get = function(controller, action, data, success, beforeSend){
         $.ajax({
             url: "/" + controller + "/" +action,
             type:'get',
             dataType:'json',
             data: data,
             beforeSend: beforeSend,
             success: success,
             error: function(a, b, c){
                 _me.log(a + b + c, 'danger');
             }
         });
     };
    _me.post = function(controller, action, data, success, beforeSend){
        $.ajax({
            url: "/" + controller + "/" +action,
            type:'post',
            dataType:'json',
            data: data,
            beforeSend: beforeSend,
            success: success,
            error: function(a, b, c){
                console.error(a + b + c, 'danger');
            }
        });
    };
    _me.log = function(msg, alertType){
        console.log(msg);
    };
    _me.dgry_time_to_local_time = function(dgry_time){
        var d1 = dgry_time;
        var d2 = new Date(d1.year, d1.month - 1, d1.day, d1.hour, d1.minute, d1.second);
        var offsetMinutes = new Date().getTimezoneOffset();// +8时区拿到的是 -480
        d2.setMinutes(d2.getMinutes() - offsetMinutes);

        return d2;
    };
    _me.parse_time = function(text, format){

    };
    _me.format_time = function(time, format){
        format = format || 'yyyy-MM-dd HH:mm:ss';
        var o = {
            "M+" : time.getMonth()+1, //month
            "d+" : time.getDate(),    //day
            "H+" : time.getHours(),   //hour
            "m+" : time.getMinutes(), //minute
            "s+" : time.getSeconds(), //second
            "q+" : Math.floor((time.getMonth()+3)/3),  //quarter
            "S" : time.getMilliseconds() //millisecond
        }
        if(/(y+)/.test(format)){
            format=format.replace(RegExp.$1, (time.getFullYear()+"").substr(4- RegExp.$1.length));

            for(var k in o){
                if(new RegExp("("+ k +")").test(format)){
                    format = format.replace(RegExp.$1,
                            RegExp.$1.length==1? o[k] :
                            ("00"+ o[k]).substr((""+ o[k]).length));
                    }
            }

            return format;
        }
    };

    _me.dgry_time_to_string = function(dgry_time, format){
        var time = _me.dgry_time_to_local_time(dgry_time);
        return DGRY.format_time(time);
    };

    _me.render_pager = function(pagerDom, url_prefix, total, page_no, page_size, page_count, location_cb){
        init_pager(pagerDom);

        var pager = {
            "url_prefix":url_prefix,
            "total":total,
            "page_no":page_no,
            "page_size":page_size,
            "page_count":page_count
        };
        bind_pager(pager, pagerDom, location_cb);

        function init_pager(pagerDom){
            var pager_html = ['<span class="btn-group">',
                                  '<a class="btn btn-sm btn-default btn-first" href="javascript:;">首 页</a>',
                                  '<a class="btn btn-sm btn-default btn-prev" href="javascript:;">上一页</a>',
                                  '<a class="btn btn-sm btn-default btn-next" href="javascript:;">下一页</a>',
                                  '<a class="btn btn-sm btn-default btn-last" href="javascript:;">末 页</a>',
                              '</span>',
                              '第<select class="page_slt"></select>页，共',
                              '<span class="lbl-page_count"></span>页，',
                              '<span class="lbl-total"></span> 条记录，',
                              '每页<select class="page_size_slt">',
                                  '<option>5</option>',
                                  '<option>10</option>',
                                  '<option>20</option>',
                                  '<option>50</option>',
                                  '<option>100</option>',
                                  '<option>200</option>',
                              '</select>条'
                              ].join('');
            pagerDom.html(pager_html);
        };

        function bind_pager(pager, pagerDom, location_cb){
            $('.lbl-total', pagerDom).text(pager.total);
            $('.page_size_slt', pagerDom).val(pager.page_size);
            $('.lbl-page_count', pagerDom).text(pager.page_count);

            $('.btn-first', pagerDom).attr('data-pager-page_no', 1);
            $('.btn-prev', pagerDom).attr('data-pager-page_no', Math.max(1, pager.page_no - 1));
            $('.btn-next', pagerDom).attr('data-pager-page_no', Math.min(pager.page_count, pager.page_no + 1));
            $('.btn-last', pagerDom).attr('data-pager-page_no', pager.page_count);

            if(pager.page_no == 1){
                $('.btn-first,.btn-prev', pagerDom).attr('disabled','disabled');
            }
            if(pager.page_no == pager.page_count){
                $('.btn-last,.btn-next', pagerDom).attr('disabled','disabled');
            }

            $('.btn-first,.btn-prev,.btn-next,.btn-last', pagerDom).click(function(){
                var $this = $(this);
                var new_pn = $this.attr('data-pager-page_no');
                redirect(new_pn);
            });

            var options = "";
            for(var i=1; i<= pager.page_count; i++){
                options += '<option value="' + i + '">' + i + '</option>';
            }
            $('.page_slt', pagerDom).html(options).find('option[value=' + pager.page_no + ']').attr('selected','selected');
            $('.page_slt', pagerDom).change(function(){
                var new_pn = $(this).val();
                redirect(new_pn);
            });
            $('.page_size_slt', pagerDom).change(function(){
                pager.page_size = $(this).val();
                redirect(1);
            });

            function redirect(new_pn){
                pager.page_no = new_pn;
                var go = pager.url_prefix + 'page_no=' + new_pn + '&page_size=' + pager.page_size;
                if(location_cb){
                    location_cb(go, pager);
                }
                else{
                    location.href = go;
                }
            };
        };
    };
}));