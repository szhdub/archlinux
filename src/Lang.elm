module Lang exposing (..)


type alias LocalText =
    { archive : String
    , balance : String
    , done : String
    , duplicate_item_found : String
    , history : String
    , item_not_found : String
    , orders : String
    , order_number : String
    , place_order : String
    , queued : String
    , server_id : String
    , stock : String
    , trades : String
    , username : String
    , usersmanage : String
    , loading : String
    , price : String
    , error : String
    }


eng_text =
    { queued = "排队登录"
    , done = "完成"
    , history = "隐藏的订单"
    , stock = "库存列表"
    , username = "用户名"
    , balance = "交易金额"
    , duplicate_item_found = "物品价格重复"
    , item_not_found = "无法找到物品"
    , orders = "交易购买"
    , order_number = "订单号"
    , trades = "交易"
    , server_id = "服务器ID"
    , place_order = "确定购买"
    , archive = "隐藏"
    , loading = "loading"
    , price = "price"
    , error = "error"
    , usersmanage = "用户管理"
    }


zh_text =
    { queued = "queued"
    , done = "done"
    , history = "history"
    , stock = "stock"
    , username = "username"
    , balance = "balance"
    , duplicate_item_found = "duplicate item found"
    , item_not_found = "item not found"
    , orders = "orders"
    , order_number = "order number"
    , trades = "trades"
    , server_id = "server id"
    , place_order = "place order"
    , archive = "archive"
    , loading = "loading"
    , price = "price"
    , error = "Error"
    , usersmanage = "users management"
    }


type alias Login_Token =
    { token : String
    }


login_token =
    { token = ""
    }
