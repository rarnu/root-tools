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
package weibo4android;

import weibo4android.http.HttpClient;

/**
 * @author Yusuke Yamamoto - yusuke at mac.com
 */
/*protected*/ class WeiboSupport {
    protected HttpClient http = new HttpClient();
    protected String source = Configuration.getSource();
    protected final boolean USE_SSL;

    /*package*/ WeiboSupport(){
        this(null, null);
    }
    /*package*/ WeiboSupport(String userId, String password){
        USE_SSL = Configuration.useSSL();
        setClientVersion(null);
        setClientURL(null);
    }

    /**
     * Sets the User-Agent header. System property -Dsinat4j.http.userAgent overrides this attribute.
     * @param userAgent UserAgent
     * @since Weibo4J 1.2.0
     */
    public void setUserAgent(String userAgent){
        http.setUserAgent(userAgent);
    }

    /**
     *
     * @return UserAgent
     * @since Weibo4J 1.2.0
     */
    public String getUserAgent(){
        return http.getUserAgent();
    }

    /**
     * Sets the X-Weibo-Client-Version header. System property -Dsinat4j.clientVersion overrides this attribute.
     * @param version client version
     * @since Weibo4J 1.2.0
     */
    public void setClientVersion(String version){
        setRequestHeader("X-Weibo-Client-Version", Configuration.getCilentVersion(version));
    }

    /**
     *
     * @return client version
     * @since Weibo4J 1.2.0
     */
    public String getClientVersion(){
        return http.getRequestHeader("X-Weibo-Client-Version");
    }

    /**
     * Sets the X-Weibo-Client-URL header. System property -Dsinat4j.clientURL overrides this attribute.
     * @param clientURL client URL
     * @since Weibo4J 1.2.0
     */
    public void setClientURL(String clientURL){
        setRequestHeader("X-Weibo-Client-URL", Configuration.getClientURL(clientURL));
    }

    /**
     *
     * @return client URL
     * @since Weibo4J 1.2.0
     */
    public String getClientURL(){
        return http.getRequestHeader("X-Weibo-Client-URL");
    }

    /**
     * Returns authenticating userid
     *
     * @return userid
     */
    public String getUserId() {
        return http.getUserId();
    }

    /**
     * Returns authenticating password
     *
     * @return password
     */
    public String getPassword() {
        return http.getPassword();
    }

    /**
     * Enables use of HTTP proxy
     *
     * @param proxyHost proxy host, can be overridden system property -Dsinat4j.http.proxyHost , -Dhttp.proxyHost
     * @param proxyPort proxy port, can be overridden system property -Dsinat4j.http.proxyPort , -Dhttp.proxyPort
     * @since Weibo4J 1.2.0
     */
    public void setHttpProxy(String proxyHost, int proxyPort) {
        http.setProxyHost(proxyHost);
        http.setProxyPort(proxyPort);
    }

    /**
     * Adds authentication on HTTP proxy
     *
     * @param proxyUser proxy user, can be overridden system property -Dsinat4j.http.proxyUser
     * @param proxyPass proxy password, can be overridden system property -Dsinat4j.http.proxyPassword
     * @since Weibo4J 1.2.0
     */
    public void setHttpProxyAuth(String proxyUser, String proxyPass) {
        http.setProxyAuthUser(proxyUser);
        http.setProxyAuthPassword(proxyPass);
    }

    /**
     * Sets a specified timeout value, in milliseconds, to be used when opening a communications link to the Weibo API.
     * System property -Dsinat4j.http.connectionTimeout overrides this attribute.
     *
     * @param connectionTimeout an int that specifies the connect timeout value in milliseconds
     * @since Weibo4J 1.2.0
     */
    public void setHttpConnectionTimeout(int connectionTimeout) {
        http.setConnectionTimeout(connectionTimeout);
    }

    /**
     * Sets the read timeout to a specified timeout, in milliseconds.
     *
     * @param readTimeoutMilliSecs an int that specifies the timeout value to be used in milliseconds
     * @since Weibo4J 1.2.0
     */
    public void setHttpReadTimeout(int readTimeoutMilliSecs) {
        http.setReadTimeout(readTimeoutMilliSecs);
    }

    /**
     * Sets X-Weibo-Client http header and the source parameter that will be passed by updating methods. System property -Dsinat4j.source overrides this attribute.
     * System property -Dsinat4j.source overrides this attribute.
     *
     * @param source the new source
     */
    public void setSource(String source) {
        this.source = Configuration.getSource(source);
        setRequestHeader("X-Weibo-Client", this.source);
    }

    /**
     * Returns the source
     *
     * @return source
     */
    public String getSource() {
        return this.source;
    }

    /**
     * Sets the request header name/value combination
     * see Weibo Fan Wiki for detail.
     * @param name  the name of the request header
     * @param value the value of the request header
     */
    public void setRequestHeader(String name, String value) {
        http.setRequestHeader(name, value);
    }

    /**
     * Set true to force using POST method communicating to the server.<br>
     * This method doesn't take effect anymore
     *
     * @param forceUsePost if true POST method will be used forcibly
     * @deprecated some methods don't accept POST method anymore
     */
    public void forceUsePost(boolean forceUsePost) {
        // this method doesn't take effect anymore
    }

    /**
     * @return true if POST is used forcibly
     */
    public boolean isUsePostForced() {
        return false;
    }

    public void setRetryCount(int retryCount) {
        http.setRetryCount(retryCount);
    }

    public void setRetryIntervalSecs(int retryIntervalSecs) {
        http.setRetryIntervalSecs(retryIntervalSecs);
    }
}
