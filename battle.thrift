 /*
 notice: 请注意命名, 协议会对返回值自动生成一个struct, struct的名字是 rpc名字 + _result,
 例如: i32 add_player_resource(1:i32 gold, 2:i32 diamond), 这个函数会自动生成 add_player_resource_result的struct
                     因此定义一个函数的返回值的时候, 需要避免和自动生成工具中的代码重名
  */

const i32 battle_version = 1;

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

service battle {
    i32 start_battle(1:i64 player_id);
}
