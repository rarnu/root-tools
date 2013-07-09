package com.sbbs.me.android.api;

import java.util.Date;

import org.eclipse.egit.github.core.User;

public class SbbsMeGithubUser extends User {

	private static final long serialVersionUID = 3515761440571300915L;

	public Date createdAt;
	public int followers;
	public int following;
	public int id;
	public String avatarUrl;
	public String blog;
	public String company;
	public String email;
	public String gravatarId;
	public String htmlUrl;
	public String location;
	public String login;
	public String name;
	public String url;

	public static SbbsMeGithubUser fromParent(User user) {
		SbbsMeGithubUser u = null;
		if (user != null) {
			u = new SbbsMeGithubUser();
			u.createdAt = user.getCreatedAt();
			u.followers = user.getFollowers();
			u.following = user.getFollowing();
			u.id = user.getId();
			u.avatarUrl = user.getAvatarUrl();
			u.blog = user.getBlog();
			u.company = user.getCompany();
			u.email = user.getEmail();
			u.gravatarId = user.getGravatarId();
			u.htmlUrl = user.getHtmlUrl();
			u.location = user.getLocation();
			u.login = user.getLogin();
			u.name = user.getName();
			u.url = user.getUrl();
		}
		return u;
	}

}
