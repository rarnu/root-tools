package com.tencent.weibo.beans;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

 

/**
 * OAuth认证参数实体类
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

public class OAuth {
	private String oauth_consumer_key = "801096959";// AppKey(client credentials)	
	private String oauth_consumer_secret = "a80271cd8dd8026578f187f376e81cf2";// 签名密钥1
	private String oauth_signature_method = "HMAC-SHA1";// 签名方法，暂只支持HMAC-SHA1
	private String oauth_timestamp = "";// 时间戳
	private String oauth_nonce = "";// 单次值，随机字符串，防止重放攻击
	private String oauth_callback = "null";// 认证成功后浏览器会被重定向到这个url中
	private String oauth_version = "1.0";// (可选)
	private String oauth_token = "";	//授权码
	private String oauth_token_secret = ""; // 签名密钥2
	private String oauth_verifier = ""; //验证码
	private int status = 0;//认证状态,0:成功,1:Request失败,2:Access失败
	private Account account = new Account(); // 个人账户信息	
	private String msg = "";
	private Random random = new Random();
	
	public OAuth(){
		super();
	}
	
	public OAuth(String oauth_callback) {
		super();
		this.oauth_callback = oauth_callback;
	}
	
	public OAuth(String oauth_consumer_key,String oauth_consumer_secret,String oauth_callback) {
		super();
		this.oauth_consumer_key = oauth_consumer_key;
		this.oauth_consumer_secret = oauth_consumer_secret;
		this.oauth_callback = oauth_callback;
	}	

	public List<QParameter> getParams() {
		List<QParameter> parameters = new ArrayList<QParameter>();
		oauth_timestamp = this.generateTimeStamp();
		oauth_nonce = this.generateNonce();
		if(null!=oauth_consumer_key && !"".equals(oauth_consumer_key.trim()))
			parameters.add(new QParameter("oauth_consumer_key", oauth_consumer_key));
		if(null!=oauth_signature_method && !"".equals(oauth_signature_method.trim()))
			parameters.add(new QParameter("oauth_signature_method", oauth_signature_method));
		if(null!=oauth_timestamp && !"".equals(oauth_timestamp.trim()))
			parameters.add(new QParameter("oauth_timestamp", oauth_timestamp));
		if(null!=oauth_nonce && !"".equals(oauth_nonce.trim()))
			parameters.add(new QParameter("oauth_nonce", oauth_nonce));
		if(null!=oauth_callback && !"".equals(oauth_callback.trim()))
			parameters.add(new QParameter("oauth_callback", oauth_callback));
		if(null!=oauth_version && !"".equals(oauth_version.trim()))
			parameters.add(new QParameter("oauth_version", oauth_version));			
		return parameters;
	}
	
	public List<QParameter> getAccessParams(){		
		List<QParameter> parameters = this.getTokenParams();
		if(oauth_verifier!=null && !"".equals(oauth_verifier)){
			parameters.add(new QParameter("oauth_verifier", oauth_verifier));
		}
		return parameters;
	}
	
	public List<QParameter> getTokenParams() {
		List<QParameter> parameters = new ArrayList<QParameter>();
		oauth_timestamp = this.generateTimeStamp();
		oauth_nonce = this.generateNonce();
		parameters.add(new QParameter("oauth_consumer_key", oauth_consumer_key));
		parameters.add(new QParameter("oauth_signature_method", oauth_signature_method));
		parameters.add(new QParameter("oauth_timestamp", oauth_timestamp));
		parameters.add(new QParameter("oauth_nonce", oauth_nonce));
		parameters.add(new QParameter("oauth_token", oauth_token));
		parameters.add(new QParameter("oauth_version", oauth_version));			
		return parameters;
	}

	public String getOauth_consumer_key() {
		return oauth_consumer_key;
	}

	public void setOauth_consumer_key(String oauth_consumer_key) {
		this.oauth_consumer_key = oauth_consumer_key;
	}

	public String getOauth_signature_method() {
		return oauth_signature_method;
	}

	public void setOauth_signature_method(String oauth_signature_method) {
		this.oauth_signature_method = oauth_signature_method;
	}

	public String getOauth_consumer_secret() {
		return oauth_consumer_secret;
	}

	public void setOauth_consumer_secret(String oauth_consumer_secret) {
		this.oauth_consumer_secret = oauth_consumer_secret;
	}

	public String getOauth_timestamp() {
		return oauth_timestamp;
	}

	public void setOauth_timestamp(String oauth_timestamp) {
		this.oauth_timestamp = oauth_timestamp;
	}

	public String getOauth_nonce() {
		return oauth_nonce;
	}

	public void setOauth_nonce(String oauth_nonce) {
		this.oauth_nonce = oauth_nonce;
	}

	public String getOauth_callback() {
		return oauth_callback;
	}

	public void setOauth_callback(String oauth_callback) {
		this.oauth_callback = oauth_callback;
	}

	public String getOauth_version() {
		return oauth_version;
	}

	public void setOauth_version(String oauth_version) {
		this.oauth_version = oauth_version;
	}

	public String getOauth_token() {
		return oauth_token;
	}

	public void setOauth_token(String oauth_token) {
		this.oauth_token = oauth_token;
	}

	public String getOauth_token_secret() {
		return oauth_token_secret;
	}

	public void setOauth_token_secret(String oauth_token_secret) {
		this.oauth_token_secret = oauth_token_secret;
	}

	public String getOauth_verifier() {
		return oauth_verifier;
	}

	public void setOauth_verifier(String oauth_verifier) {
		this.oauth_verifier = oauth_verifier;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public Account getAccount() {
		return account;
	}

	public void setAccount(Account account) {
		this.account = account;
	}

	public String getMsg() {
		return msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}

	/**
	 * Generate the timestamp for the signature.
	 * 
	 * @return
	 */
	private String generateTimeStamp() {
		return String.valueOf(System.currentTimeMillis() / 1000);
	}

	/**
	 * Just a simple implementation of a random number between 123400 and
	 * 9999999
	 * 
	 * @return
	 */
	private String generateNonce() {
		return String.valueOf(random.nextInt(9876599) + 123400);
	}
}
