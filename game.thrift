 /*
 notice: 请注意命名, 协议会对返回值自动生成一个struct, struct的名字是 rpc名字 + _result,
 例如: i32 add_player_resource(1:i32 gold, 2:i32 diamond), 这个函数会自动生成 add_player_resource_result的struct
                     因此定义一个函数的返回值的时候, 需要避免和自动生成工具中的代码重名
  */
const i32 protocol_version = 2015092201
const string protocl_version_remarks = "login"
const i32 editor_object_type_base = 100000000
const i32 camp_army_instanceid_base = 1000
const i32 resource_block_instanceid_base = 2000

// 设备平台
enum device_platform
{
    iOS = 1;
    Android = 2;
    PC = 3;
    Other = 4;
}

enum login_exception_errcode
{
    UNKNOW_ERROR = 101001;           // 未知错误
    WRONG_PROTOCAL = 101002;         // 协议版本不一致
    WRONG_PASSWORD = 101003;         // 密码错误
    UNKNOW_ACCOUNT = 101004;         // 未知账号
    GAME_SERVER_FULL = 101005;       // 服务器已满
    GAME_SERVER_OFFLINE = 101006;    // 服务器未开启
    UNDER_ATTACKING = 101007;        // 被攻击中
}

enum set_player_name_errcode
{
    PLAYER_NOT_FOUND = 102001;           // 未找到玩家账号
    ILLIGAL_NAME_LENGTH = 102002;        // 昵称长度不合法
}

enum feedback_errcode
{
     SEND_QUICKLY = 103001;                   //反馈太快
     MESSAGE_ILLEGAL_CHARACTER = 103002;        //含有特殊字符
}

exception login_exception
{
    1:login_exception_errcode error_code;
    2:string msg;
}

exception get_player_exception
{
    1:string msg;
}

exception set_player_name_exception
{
    1:set_player_name_errcode error_code;
    2:string msg;
}


// 反馈系统异常
exception feedback_exception
{
    1:feedback_errcode error_code;
    2:string msg;
}

// 日期时间
struct datetime
{
    1:i32 year;
    2:i32 month;
    3:i32 day;
    4:i32 hour;
    5:i32 minute;
    6:i32 second;
}

// 日期
struct date
{
    1:i32 year;
    2:i32 month;
    3:i32 day;
}

// 时间
struct time
{
    4:i32 hour;
    5:i32 minute;
    6:i32 second;
}

struct another_device_logined
{
    1:string ip_address;
    2:i32 port;
}

// 停服通知的消息协议
struct stop_server_msg
{
    1:i32 rest_time;    // 离关服还剩多少时间(单位:分钟)
}

// 玩家数据<表名 player>
struct player
{
    1:i64 player_id;    // 玩家id
    2:string name;      // 玩家昵称
    3:string area;      // 玩家所在区域(国家)
}

service game {
    string get_message(1: string name);
    // 获取当前协议版本号
    i32 get_protocol_version();
    
    // 登陆接口
    // 返回值: -2: game server 服务器没开， -1: 没有这个账号, 0: 密码错误, > 0 玩家的id
    i64 login(1:i32 pro_ver, 2:string account, 3:string password) throws(
                                                    1:login_exception err1);

    // 返回值: -2: game server 服务器没开， -1: 没有这个账号, 0: 密码错误, > 0 玩家的id
    i64 login_by_gamecenter(1:i32 pro_ver, 2:string user_id, 3:string name, 4:string password, 5:string accout_platform, 6:string area) throws(
                                                    1:login_exception err1);

    i64 login_by_device(1:i32 pro_ver, 2:string device_token, 3:device_platform platform,
                                                4:string area) throws(
                                                    1:login_exception err1);

    // 发起重连
    // 返回值: -2:账号在线， -1: 没有这个账号, 0: 密码错误, > 0 玩家的id
    i64 reconnect(1:string account, 2:string password);

    i64 reconnect_by_device(1:string device_token, 2:device_platform platform, 3:string area);

    // 登出, 退出游戏, 不包括网络异常的情况
    oneway void logout();

    // 心跳
    oneway void heartbeat();

    // 获取服务器当前时间(utc)
    datetime get_server_time();

    // 获取服务器当前时间(utc) (减去 1970-1-1 0:0:0 的毫秒数)
    i64 get_server_time_tick();

    player get_player(1:i64 id);

    bool set_player_name(1:string new_player_name) throws(1:set_player_name_exception err);

    void get_exception(1: i32 id) throws(1:get_player_exception err);


    // -----------------------------------------反馈接口----------------------------------------
    void feedback(1:string message 2:string image_title 3:binary image, 4:binary log) throws(1:feedback_exception err);
}
