/*
Copyright (c) 2007-2009, Yusuke Yamamoto
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Yusuke Yamamoto nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Yusuke Yamamoto ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Yusuke Yamamoto BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package weibo4android.http;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.Authenticator;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.PasswordAuthentication;
import java.net.Proxy;
import java.net.Proxy.Type;
import java.net.URL;
import java.net.URLEncoder;
import java.security.AccessControlException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.multipart.FilePart;
import org.apache.commons.httpclient.methods.multipart.MultipartRequestEntity;
import org.apache.commons.httpclient.methods.multipart.Part;
import org.apache.commons.httpclient.methods.multipart.PartBase;
import org.apache.commons.httpclient.methods.multipart.StringPart;
import weibo4android.Configuration;
import weibo4android.Weibo;
import weibo4android.WeiboException;

/**
 * A utility class to handle HTTP request/response.
 * @author Yusuke Yamamoto - yusuke at mac.com
 */
public class HttpClient implements java.io.Serializable {
    private static final int OK = 200;// OK: Success!
    private static final int NOT_MODIFIED = 304;// Not Modified: There was no new data to return.
    private static final int BAD_REQUEST = 400;// Bad Request: The request was invalid.  An accompanying error message will explain why. This is the status code will be returned during rate limiting.
    private static final int NOT_AUTHORIZED = 401;// Not Authorized: Authentication credentials were missing or incorrect.
    private static final int FORBIDDEN = 403;// Forbidden: The request is understood, but it has been refused.  An accompanying error message will explain why.
    private static final int NOT_FOUND = 404;// Not Found: The URI requested is invalid or the resource requested, such as a user, does not exists.
    private static final int NOT_ACCEPTABLE = 406;// Not Acceptable: Returned by the Search API when an invalid format is specified in the request.
    private static final int INTERNAL_SERVER_ERROR = 500;// Internal Server Error: Something is broken.  Please post to the group so the Weibo team can investigate.
    private static final int BAD_GATEWAY = 502;// Bad Gateway: Weibo is down or being upgraded.
    private static final int SERVICE_UNAVAILABLE = 503;// Service Unavailable: The Weibo servers are up, but overloaded with requests. Try again later. The search and trend methods use this to indicate when you are being rate limited.

    private final static boolean DEBUG = Configuration.getDebug();

    private int retryCount = Configuration.getRetryCount();
    private int retryIntervalMillis = Configuration.getRetryIntervalSecs() * 1000;
    private String userId = Configuration.getUser();
    private String password = Configuration.getPassword();
    private String proxyHost = Configuration.getProxyHost();
    private int proxyPort = Configuration.getProxyPort();
    private String proxyAuthUser = Configuration.getProxyUser();
    private String proxyAuthPassword = Configuration.getProxyPassword();
    private int connectionTimeout = Configuration.getConnectionTimeout();
    private int readTimeout = Configuration.getReadTimeout();
    private static final long serialVersionUID = 808018030183407996L;
    private static boolean isJDK14orEarlier = false;
    private Map<String, String> requestHeaders = new HashMap<String, String>();
    private OAuth oauth = null;
    private String requestTokenURL = Configuration.getScheme() + "api.t.sina.com.cn/oauth/request_token";
    private String authorizationURL = Configuration.getScheme() + "api.t.sina.com.cn/oauth/authorize";
    private String authenticationURL = Configuration.getScheme() + "api.t.sina.com.cn/oauth/authenticate";
    private String accessTokenURL = Configuration.getScheme() + "api.t.sina.com.cn/oauth/access_token";
    private OAuthToken oauthToken = null;
    private String token = null;

    static {
        try {
            String versionStr = System.getProperty("java.specification.version");
            if (null != versionStr) {
                isJDK14orEarlier = 1.5d > Double.parseDouble(versionStr);
            }
        } catch (AccessControlException ace) {
            isJDK14orEarlier = true;
        }
    }

    public HttpClient() {
        setUserAgent(null);
        setOAuthConsumer(null, null);
        setRequestHeader("Accept-Encoding","gzip");
    }
    
    public String getUserId() {
        return userId;
    }

    public String getPassword() {
        return password;
    }
    
    public boolean isAuthenticationEnabled(){
        return null != oauth;
    }
   
    /**
     * Sets the consumer key and consumer secret.<br>
     * System property -Dsinat4j.oauth.consumerKey and -Dhttp.oauth.consumerSecret override this attribute.
     * @param consumerKey Consumer Key
     * @param consumerSecret Consumer Secret
     * @since Weibo4J 1.2.0
     * @see <a href="http://open.t.sina.com.cn/wiki/index.php/Oauth">Applications Using Weibo</a>
     */
    public void setOAuthConsumer(String consumerKey, String consumerSecret) {
        consumerKey = Configuration.getOAuthConsumerKey(consumerKey);
        consumerSecret = Configuration.getOAuthConsumerSecret(consumerSecret);
        if (null != consumerKey && null != consumerSecret
                && 0 != consumerKey.length() && 0 != consumerSecret.length()) {
            this.oauth = new OAuth(consumerKey, consumerSecret);
        }
    }

    public RequestToken setToken(String token, String tokenSecret) {
    	this.token = token;
    	this.oauthToken = new RequestToken(token, tokenSecret);
        return (RequestToken)this.oauthToken;
    }

    /**
     *
     * @return request token
     * @throws WeiboException tw
     * @since Weibo4J 1.2.0
     */
    public RequestToken getOAuthRequestToken() throws WeiboException {
        this.oauthToken = new RequestToken(httpRequest(requestTokenURL, null, true), this);
        return (RequestToken)this.oauthToken;
    }

    /**
     * @param callback_url callback url
     * @return request token
     * @throws WeiboException tw
     * @since Weibo4J 1.2.0
     */
    public RequestToken getOauthRequestToken(String callback_url) throws WeiboException {
        this.oauthToken = new RequestToken(httpRequest(requestTokenURL,
                new PostParameter[]{new PostParameter("oauth_callback", callback_url)}
                , true), this);
        return (RequestToken) this.oauthToken;
    }

    /**
     *
     * @param token request token
     * @return access token
     * @throws WeiboException
     * @since Weibo4J 1.2.0
     */
    public AccessToken getOAuthAccessToken(RequestToken token) throws WeiboException {
        try {
            this.oauthToken = token;
            this.oauthToken = new AccessToken(httpRequest(accessTokenURL, new PostParameter[0], true));
        } catch (WeiboException te) {
            throw new WeiboException("The user has not given access to the account.", te, te.getStatusCode());
        }
        return (AccessToken) this.oauthToken;
    }

    /**
     *
     * @param token request token
     * @return access token
     * @throws WeiboException
     * @since Weibo4J 1.2.0
     */
    public AccessToken getOAuthAccessToken(RequestToken token, String pin) throws WeiboException {
        try {
            this.oauthToken = token;
            this.oauthToken = new AccessToken(httpRequest(accessTokenURL
                    , new PostParameter[]{new PostParameter("oauth_verifier", pin)}, true));
        } catch (WeiboException te) {
            throw new WeiboException("The user has not given access to the account.", te, te.getStatusCode());
        }
        return (AccessToken) this.oauthToken;
    }

    /**
     *
     * @param token request token
     * @param tokenSecret request token secret
     * @return access token
     * @throws WeiboException
     * @since Weibo4J 1.2.0
     */
    public AccessToken getOAuthAccessToken(String token, String tokenSecret) throws WeiboException {
        try {
            this.oauthToken = new OAuthToken(token, tokenSecret) {
            };
            this.oauthToken = new AccessToken(httpRequest(accessTokenURL, new PostParameter[0], true));
        } catch (WeiboException te) {
            throw new WeiboException("The user has not given access to the account.", te, te.getStatusCode());
        }
        return (AccessToken) this.oauthToken;
    }

    /**
     *
     * @param token request token
     * @param tokenSecret request token secret
     * @param oauth_verifier oauth_verifier or pin
     * @return access token
     * @throws WeiboException
     * @since Weibo4J 1.2.0
     */
    public AccessToken getOAuthAccessToken(String token, String tokenSecret
            , String oauth_verifier) throws WeiboException {
        try {
            this.oauthToken = new OAuthToken(token, tokenSecret) {
            };
            this.oauthToken = new AccessToken(httpRequest(accessTokenURL,
                    new PostParameter[]{new PostParameter("oauth_verifier", oauth_verifier)}, true));
        } catch (WeiboException te) {
            throw new WeiboException("The user has not given access to the account.", te, te.getStatusCode());
        }
        return (AccessToken) this.oauthToken;
    }

    public AccessToken getXAuthAccessToken(String userId,String passWord,String mode) throws WeiboException {
    	this.oauthToken = new AccessToken(httpRequest(accessTokenURL,
    			new PostParameter[]{
    		new PostParameter("x_auth_username", userId)
    		,new PostParameter("x_auth_password", passWord),
    		new PostParameter("x_auth_mode", mode)
    	}, true));
    	return (AccessToken) this.oauthToken;
    }

    /**
     * Sets the authorized access token
     * @param token authorized access token
     * @since Weibo4J 2.0.0
     */

    public void setOAuthAccessToken(AccessToken token){
        this.oauthToken = token;
    }

    public void setRequestTokenURL(String requestTokenURL) {
        this.requestTokenURL = requestTokenURL;
    }

    public String getRequestTokenURL() {
        return requestTokenURL;
    }


    public void setAuthorizationURL(String authorizationURL) {
        this.authorizationURL = authorizationURL;
    }

    public String getAuthorizationURL() {
        return authorizationURL;
    }

    public String getAuthenticationURL() {
		return authenticationURL;
	}

	public void setAuthenticationURL(String authenticationURL) {
		this.authenticationURL = authenticationURL;
	}
    /**
     * since Weibo4J 1.2.0
     */
    public String getAuthenticationRL() {
        return authenticationURL;
    }

    public void setAccessTokenURL(String accessTokenURL) {
        this.accessTokenURL = accessTokenURL;
    }

    public String getAccessTokenURL() {
        return accessTokenURL;
    }

    public String getProxyHost() {
        return proxyHost;
    }

    /**
     * Sets proxy host.
     * System property -Dsinat4j.http.proxyHost or http.proxyHost overrides this attribute.
     * @param proxyHost
     */
    public void setProxyHost(String proxyHost) {
        this.proxyHost = Configuration.getProxyHost(proxyHost);
    }

    public int getProxyPort() {
        return proxyPort;
    }

    /**
     * Sets proxy port.
     * System property -Dsinat4j.http.proxyPort or -Dhttp.proxyPort overrides this attribute.
     * @param proxyPort
     */
    public void setProxyPort(int proxyPort) {
        this.proxyPort = Configuration.getProxyPort(proxyPort);
    }

    public String getProxyAuthUser() {
        return proxyAuthUser;
    }

    /**
     * Sets proxy authentication user.
     * System property -Dsinat4j.http.proxyUser overrides this attribute.
     * @param proxyAuthUser
     */
    public void setProxyAuthUser(String proxyAuthUser) {
        this.proxyAuthUser = Configuration.getProxyUser(proxyAuthUser);
    }

    public String getProxyAuthPassword() {
        return proxyAuthPassword;
    }

    /**
     * Sets proxy authentication password.
     * System property -Dsinat4j.http.proxyPassword overrides this attribute.
     * @param proxyAuthPassword
     */
    public void setProxyAuthPassword(String proxyAuthPassword) {
        this.proxyAuthPassword = Configuration.getProxyPassword(proxyAuthPassword);
    }

    public int getConnectionTimeout() {
        return connectionTimeout;
    }

    /**
     * Sets a specified timeout value, in milliseconds, to be used when opening a communications link to the resource referenced by this URLConnection.
     * System property -Dsinat4j.http.connectionTimeout overrides this attribute.
     * @param connectionTimeout - an int that specifies the connect timeout value in milliseconds
     */
    public void setConnectionTimeout(int connectionTimeout) {
        this.connectionTimeout = Configuration.getConnectionTimeout(connectionTimeout);

    }
    public int getReadTimeout() {
        return readTimeout;
    }

    /**
     * Sets the read timeout to a specified timeout, in milliseconds. System property -Dsinat4j.http.readTimeout overrides this attribute.
     * @param readTimeout - an int that specifies the timeout value to be used in milliseconds
     */
    public void setReadTimeout(int readTimeout) {
        this.readTimeout = Configuration.getReadTimeout(readTimeout);
    }

    public void setRetryCount(int retryCount) {
        if (retryCount >= 0) {
            this.retryCount = Configuration.getRetryCount(retryCount);
        } else {
            throw new IllegalArgumentException("RetryCount cannot be negative.");
        }
    }

    public void setUserAgent(String ua) {
        setRequestHeader("User-Agent", Configuration.getUserAgent(ua));
    }
    public String getUserAgent(){
        return getRequestHeader("User-Agent");
    }

    public void setRetryIntervalSecs(int retryIntervalSecs) {
        if (retryIntervalSecs >= 0) {
            this.retryIntervalMillis = Configuration.getRetryIntervalSecs(retryIntervalSecs) * 1000;
        } else {
            throw new IllegalArgumentException(
                    "RetryInterval cannot be negative.");
        }
    }

    public Response post(String url, PostParameter[] postParameters,
                         boolean authenticated) throws WeiboException {
    	PostParameter[] newPostParameters = new PostParameter[postParameters.length+1];
		for(int i = 0; i < postParameters.length; i++) {
			newPostParameters[i] = postParameters[i];
		}
    	newPostParameters[ postParameters.length]=new PostParameter("source", Weibo.CONSUMER_KEY);
        return httpRequest(url, newPostParameters, authenticated);
    }
    public Response post(String url, String key,String value,
            boolean authenticated) throws WeiboException {
    	return post(url,new PostParameter[]{new PostParameter(key, value)},authenticated);
}
    public Response delete(String url, boolean authenticated) throws WeiboException {
    	return httpRequest(url, null, authenticated, "DELETE");
    }

 	public Response multPartURL(String url,  PostParameter[] params,ImageItem item,boolean authenticated) throws WeiboException{
  		PostMethod post = new PostMethod(url);
    	try {
    		org.apache.commons.httpclient.HttpClient client = new org.apache.commons.httpclient.HttpClient();
    		long t = System.currentTimeMillis();
    		Part[] parts=null;
    		if(params==null){
    			parts=new Part[1];
    		}else{
    			parts=new Part[params.length+1];
    		}
    		if (params != null ) {
    			int i=0;
      			for (PostParameter entry : params) {
      				parts[i++]=new StringPart( entry.getName(),(String)entry.getValue());
    			}
      			parts[parts.length-1]=new ByteArrayPart(item.getContent(), item.getName(), item.getImageType());
      		}
    		post.setRequestEntity( new MultipartRequestEntity(parts, post.getParams()) );
    		 List<Header> headers = new ArrayList<Header>();

    		 if (authenticated) {
    	            if (oauth == null) {
    	            }
    	            String authorization = null;
    	            if (null != oauth) {
    	                // use OAuth
    	                authorization = oauth.generateAuthorizationHeader( "POST" , url, params, oauthToken);
    	            } else {
    	                throw new IllegalStateException(
    	                        "Neither user ID/password combination nor OAuth consumer key/secret combination supplied");
    	            }
    	            headers.add(new Header("Authorization", authorization));
    	            log("Authorization: " + authorization);
    	        }
    	    client.getHostConfiguration().getParams().setParameter("http.default-headers", headers);
    		client.executeMethod(post);

    		Response response=new Response();
    		response.setResponseAsString(post.getResponseBodyAsString());
    		response.setStatusCode(post.getStatusCode());

    		log("multPartURL URL:" + url + ", result:" + response + ", time:" + (System.currentTimeMillis() - t));
        	return response;
    	} catch (Exception ex) {
    		 throw new WeiboException(ex.getMessage(), ex, -1);
    	} finally {
    		post.releaseConnection();
    	}
  	}

 	public Response multPartURL(String fileParamName,String url,  PostParameter[] params,File file,boolean authenticated) throws WeiboException{
  		PostMethod post = new PostMethod(url);
  		org.apache.commons.httpclient.HttpClient client = new org.apache.commons.httpclient.HttpClient();
    	try {
    		long t = System.currentTimeMillis();
    		Part[] parts=null;
    		if(params==null){
    			parts=new Part[1];
    		}else{
    			parts=new Part[params.length+1];
    		}
    		if (params != null ) {
    			int i=0;
      			for (PostParameter entry : params) {
      				parts[i++]=new StringPart( entry.getName(),(String)entry.getValue());
    			}
      		}
    		FilePart filePart=new FilePart(fileParamName,file.getName(), file,new FileType().getMIMEType(file),"UTF-8");
    		filePart.setTransferEncoding("binary");
    		parts[parts.length-1]= filePart;

    		post.setRequestEntity( new MultipartRequestEntity(parts, post.getParams()) );
    		 List<Header> headers = new ArrayList<Header>();

    		 if (authenticated) {
    	            if (oauth == null) {
    	            }
    	            String authorization = null;
    	            if (null != oauth) {
    	                // use OAuth
    	                authorization = oauth.generateAuthorizationHeader( "POST" , url, params, oauthToken);
    	            }else {
    	                throw new IllegalStateException(
    	                        "Neither user ID/password combination nor OAuth consumer key/secret combination supplied");
    	            }
    	            headers.add(new Header("Authorization", authorization));
    	            log("Authorization: " + authorization);
    	        }
    	    client.getHostConfiguration().getParams().setParameter("http.default-headers", headers);
    		client.executeMethod(post);

    		Response response=new Response();
    		response.setResponseAsString(post.getResponseBodyAsString());
    		response.setStatusCode(post.getStatusCode());

    		log("multPartURL URL:" + url + ", result:" + response + ", time:" + (System.currentTimeMillis() - t));
        	return response;
    	} catch (Exception ex) {
    		 throw new WeiboException(ex.getMessage(), ex, -1);
    	} finally {
    		post.releaseConnection();
    		client=null;
    	}
  	}
 	private static class ByteArrayPart extends PartBase {
		private byte[] mData;
		private String mName;
		public ByteArrayPart(byte[] data, String name, String type) throws IOException {
			super(name, type, "UTF-8", "binary");
			mName = name;
			mData = data;
		}
		protected void sendData(OutputStream out) throws IOException {
			out.write(mData);
		}
		protected long lengthOfData() throws IOException {
			return mData.length;
		}
	    protected void sendDispositionHeader(OutputStream out) throws IOException {
	    	super.sendDispositionHeader(out);
	    	StringBuilder buf = new StringBuilder();
	    	buf.append("; filename=\"").append(mName).append("\"");
	    	out.write(buf.toString().getBytes());
	    }
	}

    public Response post(String url, boolean authenticated) throws WeiboException {
        return httpRequest(url, new PostParameter[0], authenticated);
    }

    public Response post(String url, PostParameter[] PostParameters) throws
            WeiboException {
        return httpRequest(url, PostParameters, false);
    }

    public Response post(String url) throws
            WeiboException {
        return httpRequest(url, new PostParameter[0], false);
    }

    public Response get(String url, boolean authenticated) throws
            WeiboException {
        return httpRequest(url, null, authenticated);
    }

    public Response get(String url) throws WeiboException {
        return httpRequest(url, null, false);
    }

    protected Response httpRequest(String url, PostParameter[] postParams,
            boolean authenticated) throws WeiboException {
    	//统一增加source 参数
		int len = 1;
		PostParameter[] newPostParameters = postParams;
    	String method = "GET";
    	if (postParams != null) {
    		method = "POST";
			len = postParams.length + 1;
			newPostParameters = new PostParameter[len];
			for(int i = 0; i < postParams.length; i++) {
				newPostParameters[i] = postParams[i];
			}
			newPostParameters[postParams.length] = new PostParameter("source",
					Weibo.CONSUMER_KEY);
    	}
    	return httpRequest(url, newPostParameters, authenticated, method);
    }

    public Response httpRequest(String url, PostParameter[] postParams,
                                 boolean authenticated, String httpMethod) throws WeiboException {
        int retriedCount;
        int retry = retryCount + 1;
        Response res = null;
        for (retriedCount = 0; retriedCount < retry; retriedCount++) {
            int responseCode = -1;
            try {
                HttpURLConnection con = null;
                OutputStream osw = null;
                try {
                    con = getConnection(url);
                    con.setDoInput(true);
                    setHeaders(url, postParams, con, authenticated, httpMethod);
                    if (null != postParams || "POST".equals(httpMethod)) {
                        con.setRequestMethod("POST");
                        con.setRequestProperty("Content-Type",
                                "application/x-www-form-urlencoded");
                        con.setDoOutput(true);
                        String postParam = "";
                        if (postParams != null) {
                        	postParam = encodeParameters(postParams);
                        }
                        log("Post Params: ", postParam);
                        byte[] bytes = postParam.getBytes("UTF-8");

                        con.setRequestProperty("Content-Length",
                                Integer.toString(bytes.length));
                        
                        osw = con.getOutputStream();
                        osw.write(bytes);
                        osw.flush();
                        osw.close();
                    } else if ("DELETE".equals(httpMethod)){
                        con.setRequestMethod("DELETE");
                    } else {
                        con.setRequestMethod("GET");
                    }
                   
                   
                    res = new Response(con);
                    responseCode = con.getResponseCode();
                    if(DEBUG){
                        log("Response: ");
                        Map<String, List<String>> responseHeaders = con.getHeaderFields();
                        for (String key : responseHeaders.keySet()) {
                            List<String> values = responseHeaders.get(key);
                            for (String value : values) {
                                if(null != key){
                                    log(key + ": " + value);
                                }else{
                                    log(value);
                                }
                            }
                        }
                    }
                    if (responseCode != OK) {
                        if (responseCode < INTERNAL_SERVER_ERROR || retriedCount == retryCount) {
                            throw new WeiboException(getCause(responseCode) + "\n" + res.asString(), responseCode);
                        }
                        // will retry if the status code is INTERNAL_SERVER_ERROR
                    } else {
                        break;
                    }
                } finally {
                    try {
                        osw.close();
                    } catch (Exception ignore) {
                    }
                }
            } catch (IOException ioe) {
                // connection timeout or read timeout
                if (retriedCount == retryCount) {
                    throw new WeiboException(ioe.getMessage(), ioe, responseCode);
                }
            }
            try {
                if(DEBUG && null != res){
                    res.asString();
                }
                log("Sleeping " + retryIntervalMillis +" millisecs for next retry.");
                Thread.sleep(retryIntervalMillis);
            } catch (InterruptedException ignore) {
                //nothing to do
            }
        }
        return res;
    }

    public static String encodeParameters(PostParameter[] postParams) {
        StringBuffer buf = new StringBuffer();
        for (int j = 0; j < postParams.length; j++) {
            if (j != 0) {
                buf.append("&");
            }
            try {
                buf.append(URLEncoder.encode(postParams[j].name, "UTF-8"))
                        .append("=").append(URLEncoder.encode(postParams[j].value, "UTF-8"));
            } catch (java.io.UnsupportedEncodingException neverHappen) {
            }
        }
        return buf.toString();

    }

    /**
     * sets HTTP headers
     *
     * @param connection    HttpURLConnection
     * @param authenticated boolean
     */
    private void setHeaders(String url, PostParameter[] params, HttpURLConnection connection, boolean authenticated, String httpMethod) {
        log("Request: ");
        log(httpMethod + " ", url);

        if (authenticated) {
            if (oauth == null) {
            }
            String authorization = null;
            if (null != oauth) {
                // use OAuth
                authorization = oauth.generateAuthorizationHeader(httpMethod, url, params, oauthToken);
            }else {
                throw new IllegalStateException(
                        "Neither user ID/password combination nor OAuth consumer key/secret combination supplied");
            }
            connection.addRequestProperty("Authorization", authorization);
            log("Authorization: " + authorization);
        }
        for (String key : requestHeaders.keySet()) {
            connection.addRequestProperty(key, requestHeaders.get(key));
            log(key + ": " + requestHeaders.get(key));
        }
    }

    public void setRequestHeader(String name, String value) {
        requestHeaders.put(name, value);
    }

    public String getRequestHeader(String name) {
        return requestHeaders.get(name);
    }

    private HttpURLConnection getConnection(String url) throws IOException {
        HttpURLConnection con = null;
        if (proxyHost != null && !proxyHost.equals("")) {
            if (proxyAuthUser != null && !proxyAuthUser.equals("")) {
                log("Proxy AuthUser: " + proxyAuthUser);
                log("Proxy AuthPassword: " + proxyAuthPassword);
                Authenticator.setDefault(new Authenticator() {
                    @Override
                    protected PasswordAuthentication
                    getPasswordAuthentication() {
                        //respond only to proxy auth requests
                        if (getRequestorType().equals(RequestorType.PROXY)) {
                            return new PasswordAuthentication(proxyAuthUser,
                                    proxyAuthPassword
                                            .toCharArray());
                        } else {
                            return null;
                        }
                    }
                });
            }
            final Proxy proxy = new Proxy(Type.HTTP, InetSocketAddress
                    .createUnresolved(proxyHost, proxyPort));
            if(DEBUG){
                log("Opening proxied connection(" + proxyHost + ":" + proxyPort + ")");
            }
            con = (HttpURLConnection) new URL(url).openConnection(proxy);
        } else {
            con = (HttpURLConnection) new URL(url).openConnection();
        }
        if (connectionTimeout > 0 && !isJDK14orEarlier) {
            con.setConnectTimeout(connectionTimeout);
        }
        if (readTimeout > 0 && !isJDK14orEarlier) {
            con.setReadTimeout(readTimeout);
        }
        return con;
    }

    @Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((accessTokenURL == null) ? 0 : accessTokenURL.hashCode());
		result = prime
				* result
				+ ((authenticationURL == null) ? 0 : authenticationURL
						.hashCode());
		result = prime
				* result
				+ ((authorizationURL == null) ? 0 : authorizationURL.hashCode());
		result = prime * result + connectionTimeout;
		result = prime * result + ((oauth == null) ? 0 : oauth.hashCode());
		result = prime * result
				+ ((oauthToken == null) ? 0 : oauthToken.hashCode());
		result = prime
				* result
				+ ((proxyAuthPassword == null) ? 0 : proxyAuthPassword
						.hashCode());
		result = prime * result
				+ ((proxyAuthUser == null) ? 0 : proxyAuthUser.hashCode());
		result = prime * result
				+ ((proxyHost == null) ? 0 : proxyHost.hashCode());
		result = prime * result + proxyPort;
		result = prime * result + readTimeout;
		result = prime * result
				+ ((requestHeaders == null) ? 0 : requestHeaders.hashCode());
		result = prime * result
				+ ((requestTokenURL == null) ? 0 : requestTokenURL.hashCode());
		result = prime * result + retryCount;
		result = prime * result + retryIntervalMillis;
		result = prime * result + ((token == null) ? 0 : token.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		HttpClient other = (HttpClient) obj;
		if (accessTokenURL == null) {
			if (other.accessTokenURL != null)
				return false;
		} else if (!accessTokenURL.equals(other.accessTokenURL))
			return false;
		if (authenticationURL == null) {
			if (other.authenticationURL != null)
				return false;
		} else if (!authenticationURL.equals(other.authenticationURL))
			return false;
		if (authorizationURL == null) {
			if (other.authorizationURL != null)
				return false;
		} else if (!authorizationURL.equals(other.authorizationURL))
			return false;
		if (connectionTimeout != other.connectionTimeout)
			return false;
		if (oauth == null) {
			if (other.oauth != null)
				return false;
		} else if (!oauth.equals(other.oauth))
			return false;
		if (oauthToken == null) {
			if (other.oauthToken != null)
				return false;
		} else if (!oauthToken.equals(other.oauthToken))
			return false;
		if (proxyAuthPassword == null) {
			if (other.proxyAuthPassword != null)
				return false;
		} else if (!proxyAuthPassword.equals(other.proxyAuthPassword))
			return false;
		if (proxyAuthUser == null) {
			if (other.proxyAuthUser != null)
				return false;
		} else if (!proxyAuthUser.equals(other.proxyAuthUser))
			return false;
		if (proxyHost == null) {
			if (other.proxyHost != null)
				return false;
		} else if (!proxyHost.equals(other.proxyHost))
			return false;
		if (proxyPort != other.proxyPort)
			return false;
		if (readTimeout != other.readTimeout)
			return false;
		if (requestHeaders == null) {
			if (other.requestHeaders != null)
				return false;
		} else if (!requestHeaders.equals(other.requestHeaders))
			return false;
		if (requestTokenURL == null) {
			if (other.requestTokenURL != null)
				return false;
		} else if (!requestTokenURL.equals(other.requestTokenURL))
			return false;
		if (retryCount != other.retryCount)
			return false;
		if (retryIntervalMillis != other.retryIntervalMillis)
			return false;
		if (token == null) {
			if (other.token != null)
				return false;
		} else if (!token.equals(other.token))
			return false;
		return true;
	}

	private static void log(String message) {
        if (DEBUG) {
            System.out.println("[" + new java.util.Date() + "]" + message);
        }
    }

    private static void log(String message, String message2) {
        if (DEBUG) {
            log(message + message2);
        }
    }

    private static String getCause(int statusCode){
        String cause = null;
        switch(statusCode){
            case NOT_MODIFIED:
                break;
            case BAD_REQUEST:
                cause = "The request was invalid.  An accompanying error message will explain why. This is the status code will be returned during rate limiting.";
                break;
            case NOT_AUTHORIZED:
                cause = "Authentication credentials were missing or incorrect.";
                break;
            case FORBIDDEN:
                cause = "The request is understood, but it has been refused.  An accompanying error message will explain why.";
                break;
            case NOT_FOUND:
                cause = "The URI requested is invalid or the resource requested, such as a user, does not exists.";
                break;
            case NOT_ACCEPTABLE:
                cause = "Returned by the Search API when an invalid format is specified in the request.";
                break;
            case INTERNAL_SERVER_ERROR:
                cause = "Something is broken.  Please post to the group so the Weibo team can investigate.";
                break;
            case BAD_GATEWAY:
                cause = "Weibo is down or being upgraded.";
                break;
            case SERVICE_UNAVAILABLE:
                cause = "Service Unavailable: The Weibo servers are up, but overloaded with requests. Try again later. The search and trend methods use this to indicate when you are being rate limited.";
                break;
            default:
                cause = "";
        }
        return statusCode + ":" + cause;
    }
}
