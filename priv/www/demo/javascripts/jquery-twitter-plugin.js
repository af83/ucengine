/**
 * @author Stéphane Roucheray
 * @extends jQuery
 */


(function($){

var base_url    = "http://twitter.com/",
search_base_url = "http://search.twitter.com/",
urls = {
	search         : search_base_url + "search",
	help_test      : base_url + "help/test",
	
	trends         : search_base_url + "trends",
	trends_current : search_base_url + "trends/current",
	trends_daily   : search_base_url + "trends/daily",
	trends_weekly  : search_base_url + "trends/weekly",
	
	statuses_public_timeline  : base_url + "statuses/public_timeline",
	statuses_friends_timeline : base_url + "statuses/friends_timeline",
	statuses_user_timeline    : base_url + "statuses/user_timeline",
	statuses_mentions         : base_url + "statuses/mentions",
	statuses_show             : base_url + "statuses/show",
	statuses_update           : base_url + "statuses/update",
	statuses_destroy          : base_url + "statuses/destroy",
	statuses_friends          : base_url + "statuses/friends",
	statuses_followers        : base_url + "statuses/followers",
	
	users_show : base_url + "users/show",
	
	direct_messages      : base_url + "direct_messages",
	direct_messages_sent : base_url + "direct_messages/sent",
	direct_messages_new  : base_url + "direct_messages/new",
	direct_messages      : base_url + "direct_messages/destroy",
	
	friendship_create    : base_url + "friendships/create",
	friendship_destroy   : base_url + "friendships/destroy",
	friendship_exists    : base_url + "friendships/exists",
	
	friends_ids          : base_url + "friends/ids",
	followers_ids        : base_url + "followers/ids",
	
	account_verify_credentials               : base_url + "account/verify_credentials",
	account_rate_limit_status                : base_url + "account/rate_limit_status",
	account_end_session                      : base_url + "account/end_session",
	account_update_delivery_device           : base_url + "account/update_delivery_device",
	account_update_profile_colors            : base_url + "account/update_profile_colors",
	account_update_profile_image             : base_url + "account/update_profile_image",
	account_update_profile_background_image  : base_url + "account/",
	account_update_profile                   : base_url + "account/update_profile/update_profile_background_image",
	
	favorites              : base_url + "favorites",
	favorites_create       : base_url + "favorites/create/",
	favorites_destroy      : base_url + "favorites/destroy",
	 
	notifications_follow   : base_url + "notifications/follow",
	notifications_leave    : base_url + "notifications/leave",
	
	blocks_create          : base_url + "blocks/create",
	blocks_destroy         : base_url + "blocks/destroy",
	blocks_exists          : base_url + "blocks/exists",
	blocks_blocking        : base_url + "blocks/blocking",
	blocks_ids             : base_url + "blocks/blocking/ids",
	
	saved_searches         : base_url + "saved_searches",
	saved_searches_show    : base_url + "saved_searches/show",
	saved_searches_create  : base_url + "saved_searches/create",
	saved_searches_destroy : base_url + "saved_searches/destroy",
	
	oauth_request_token    : base_url + "oauth/request_token",
	oauth_authorize        : base_url + "oauth/authorize",
	oauth_authicate     : base_url + "oauth/authicate",
	oauth_access_token     : base_url + "oauth/access_token"
	
};

//Generic private methods to build requests
// parameters : $.ajax method specific parameters
// data : data encoded in the get url string
function request(parameters /* Object */, data /* Object */){
	parameters = $.extend({
		data:data
	}, parameters);
	
	parameters.url = parameters.url + ".json";

	//Bug in jQuery with jsonp, if "jsonp" plugin is present then use it  
	if ($.jsonp && $.isFunction($.jsonp)) {
		parameters.url += "?callback=?";
		$.jsonp(parameters);
	}else{
		parameters.dataType = "jsonp";
		$.ajax( parameters );
	}
};

// Delete all parameters Object properties not specified in the authorized Array
function cleanupParam(parameters /* Object */, authorized /* Array */){
	if (parameters) {
		$.each(parameters, function(key, value){
			if ($.inArray(key, authorized) == -1) {
				delete parameters[key];
			}
		});
	}
	
	return parameters || {};
}

// Delete non wanted 
function getAjaxParam(options, url, success){
	var ajaxParams = [
		"async",
		"cache",
		"complete",
		"contentType",
	//	"data",   // In our case, data is set outside this object
		"dataFilter",
	//	"dataType", // Always jsonp
		"error",
		"global",
		"ifModified",
	//	"jsonp", // callback name must not be renamed
		"password",
		"processData",
		"scriptCharset",
	//	"success", // In our case, success is set outside this object
		"timeout",
	//	"type", // Type, GET or POST, is determined by the calling method
	//	"url", //  User not need to set the url
		"username",
		"xhr"],
		
	output = {};
	if (ajaxParams) {
		//Delete extra parameters
		$.each(options, function(key, value){
			if ($.inArray(key, ajaxParams) != -1) {
				output[key] = value;
			}
		});
	}
	
	output.url = url;
	output.success = success;
	
	return output;
}

/* 
 * Generic twitter object method 
 */
$.twitter = {

	/* Help Methods */
	test : function(callback, options){
		var ajaxParam = getAjaxParam(options, urls.help_test, callback);
		request(ajaxParam);
	},

	/* Search API Methods */ 
	search : function(query, callback, options){
		// From options, extract jQuery Ajax param
		// And add the url and the callback method 
		var ajaxParam = getAjaxParam(options, urls.search, callback);
		
		// From options cleanup non Twitter params
		options = cleanupParam(
			options, 
			["callback","lang","rpp","page","since_id","geocode","show_user"]
		);
					
		// Limit search to 140 characters
		// And add the query to the options
		options = $.extend(
			{
				q : query.substr(0, 140)
			},options
		);
		
		// Make the request
		request(ajaxParam, options);
	},
	
	/* Trends main method */
	trends : function(callback, options){
		var ajaxParam = getAjaxParam(options, urls.trends, callback);
		request(ajaxParam);
	},
	
	statuses : {}, //No direct function
	
	users : {}, //No direct function
	
	direct_messages : function(){}, //TODO: implements
	
	friendships : {}, //No direct function
	
	friends : {}, //No direct function
	
	followers : {}, //No direct function
	
	account : {}, //No direct function
	
	favorites : function(){}, //TODO: implements
	
	notifications : {}, //No direct function
	
	blocks : {},
	
	saved_searches : function(){}, //TODO: implements
	
	oauth : {}  //No direct function
	
};

/* Simple searches */
$.twitter.search.user = function(username, callback, options){
	$.twitter.search("from:"+username, callback, options);
};
	
$.twitter.search.repliesTo = function(username, callback, options){
	$.twitter.search("to:"+username, callback, options);
};
	
$.twitter.search.mentioned = function(username, callback, options){
	$.twitter.search("@"+username, callback, options);
};
	
$.twitter.search.hashtag = function(hashtag, callback, options){
	$.twitter.search("#"+hashtag, callback, options);
};

/* Specifics trends */
$.twitter.trends.current = function(callback, options){
	// From options, extract jQuery Ajax param
	// And add the url and the callback method 
	var ajaxParam = getAjaxParam(options, urls.trends_current, callback);
	
	// From options cleanup non Twitter params		
	options = cleanupParam(
		options, 
		["exclude"]
	);
	
	request(ajaxParam, options);
};
$.twitter.trends.daily = function(callback){//TODO: implements options
	request(
		{
			url: urls.trends_daily,
			success: callback
		}
	);
};
$.twitter.trends.weekly = function(callback){//TODO: implements options
	request(
		{
			url: urls.trends_weekly,
			success: callback
		}
	);
};

/* Timeline Methods */

$.twitter.statuses.publicTimeline = function(callback, options){
	var ajaxParam = getAjaxParam(options, urls.statuses_public_timeline, callback);
	request(ajaxParam);
};

/* Status Methods */

//Requires authication if the author of the status is protected
$.twitter.statuses.show = function(userid, callback, options){
	var ajaxParam = getAjaxParam(options, urls.statuses_show + "/" + userid, callback);
	request(ajaxParam);
};

/* User Methods */

$.twitter.users.show = function(userid, callback, options){	
	//TODO : check userid is number until
	if (userid){
		options.user_id = userid;
	}
	
	var ajaxParam = getAjaxParam(options, urls.users_show + "/" + userid, callback);
	
	options = cleanupParam(
			options, 
			["id","user_id","screen_name"]
		);

	request(ajaxParam, options);
};


$.twitter.statuses.friends = function(userid, callback, options){
	if (userid){
		options.user_id = userid;
	}
	
	var ajaxParam = getAjaxParam(options, urls.statuses_friends + "/" + userid, callback);
	
	options = cleanupParam(
			options, 
			["id","user_id","screen_name", "page"]
		);
		
	request(ajaxParam, options);
};

/* Social Graph Methods */
$.twitter.friends.ids = function(userid, callback, options){
	if (userid){
		options.user_id = userid;
	}
	
	var ajaxParam = getAjaxParam(options, urls.friends_ids + "/" + userid, callback);
	
	options = cleanupParam(
			options, 
			["id","user_id","screen_name", "page"]
		);
		
	request(ajaxParam, options);
};
$.twitter.followers.ids = function(userid, callback, options){
	if (userid){
		options.user_id = userid;
	}
	
	var ajaxParam = getAjaxParam(options, urls.followers_ids + "/" + userid, callback);
	
	options = cleanupParam(
			options, 
			["id","user_id","screen_name", "page"]
		);
		
	request(ajaxParam, options);
};

//Only for IP rate limit status
$.twitter.account.rateLimitStatus = function(callback, options){
	var ajaxParam = getAjaxParam(options, urls.account_rate_limit_status, callback);
	
	request(ajaxParam);
};

})(jQuery);
