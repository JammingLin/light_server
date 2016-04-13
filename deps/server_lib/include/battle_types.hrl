-ifndef(_battle_types_included).
-define(_battle_types_included, yeah).

%% struct datetime

-record(datetime, {year :: integer(),
                   month :: integer(),
                   day :: integer(),
                   hour :: integer(),
                   minute :: integer(),
                   second :: integer()}).

%% struct date

-record(date, {year :: integer(),
               month :: integer(),
               day :: integer()}).

%% struct time

-record(time, {hour :: integer(),
               minute :: integer(),
               second :: integer()}).

-endif.
