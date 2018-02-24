-ifndef(plain_vulkan_hrl).
-define(plain_vulkan_hrl, 'true').


-define(WORKER(Name), #{'id'=>Name, 'start'=>{Name, 'start_link', []}, 'shutdown'=>2000, 'type'=>'worker', 'modules'=>[Name]}).
-define(SUPER(Name), #{'id'=>Name, 'start'=>{Name, 'start_link', []}, 'shutdown'=>'infinity', 'type'=>'supervisor', 'modules'=>[Name]}).

-type terminate_reason() :: 'normal' | 'shutdown' | {'shutdown', term()} | term().
-type child_type() :: 'worker' | 'supervisor'.

-endif.