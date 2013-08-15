package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.RelativeLayout;
import android.widget.Toast;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.intf.OnPullDownListener;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.ArticleActivity;
import com.sbbs.me.android.GalleryActivity;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.PrivateMessageActivity;
import com.sbbs.me.android.R;
import com.sbbs.me.android.UserDetailActivity;
import com.sbbs.me.android.adapter.SbbsMeArticleAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.api.SbbsMeGithubUser;
import com.sbbs.me.android.api.SbbsMeGoogleUser;
import com.sbbs.me.android.api.SbbsMeLogs;
import com.sbbs.me.android.api.SbbsMeSinaUser;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.dialog.SelectLoginDialog;
import com.sbbs.me.android.loader.SbbsBlockLoader;
import com.sbbs.me.android.utils.Config;
import com.sbbs.me.android.utils.GithubOAuth;
import com.sbbs.me.android.utils.GithubOAuth.GithubUserCallback;
import com.sbbs.me.android.utils.GoogleOAuth;
import com.sbbs.me.android.utils.GoogleOAuth.GoogleUserCallback;
import com.sbbs.me.android.utils.MiscUtils;
import com.sbbs.me.android.utils.SinaOAuth;
import com.sbbs.me.android.utils.SinaOAuth.SinaUserCallback;

public class MainFragment extends BaseFragment implements
		OnLoadCompleteListener<List<SbbsMeBlock>>, OnPullDownListener,
		OnItemClickListener, SinaUserCallback, GoogleUserCallback,
		GithubUserCallback, OnClickListener {

	PullDownListView lvPullDown;
	SbbsBlockLoader loader;
	SbbsMeArticleAdapter adapter;
	TextView tvLoading;
	TextView tvNodata;
	RelativeLayout layLogining;

	MenuItem miUser;
	MenuItem miGallery;
	MenuItem miMessage;
	SinaOAuth sinaOAuth;
	GoogleOAuth googleOAuth;
	GithubOAuth githubOAuth;

	int page = 1;
	private static final int PAGE_SIZE = 20;
	boolean isBottom = false;

	public MainFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_main_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		lvPullDown = (PullDownListView) innerView.findViewById(R.id.lvPullDown);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
		tvNodata = (TextView) innerView.findViewById(R.id.tvNodata);
		layLogining = (RelativeLayout) innerView.findViewById(R.id.layLogining);
		if (Global.listArticle == null) {
			Global.listArticle = new ArrayList<SbbsMeBlock>();
		}
		adapter = new SbbsMeArticleAdapter(getActivity(), Global.listArticle);
		lvPullDown.getListView().setAdapter(adapter);
		loader = new SbbsBlockLoader(getActivity());
		lvPullDown.enableAutoFetchMore(true, 1);

		int devide = UIUtils.dipToPx(8);
		lvPullDown.getListView().setDivider(null);
		lvPullDown.getListView().setDividerHeight(devide);
		lvPullDown.getListView().setPadding(devide, devide, devide, devide);
		lvPullDown.getListView().setSelector(R.color.transparent);
		lvPullDown.getListView().setOverScrollMode(View.OVER_SCROLL_NEVER);

		sinaOAuth = new SinaOAuth(getActivity(), this);
		googleOAuth = new GoogleOAuth(getActivity(), this);
		githubOAuth = new GithubOAuth(getActivity(), this);
	}

	@Override
	public void initEvents() {
		lvPullDown.setOnPullDownListener(this);
		lvPullDown.getListView().setOnItemClickListener(this);
		tvNodata.setOnClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {

		page = 1;
		setIsBottom(false);
		if (Global.listArticle.size() == 0 || Global.autoRefreshTag) {
			Global.autoRefreshTag = false;
			tvLoading.setVisibility(View.VISIBLE);
			loader.setRefresh(false);
			loader.setPage(page, PAGE_SIZE);
			loader.startLoading();
		} else {
			// keep the last PAGE_SIZE articles
			if (Global.listArticle.size() > PAGE_SIZE) {
				List<SbbsMeBlock> tmp = new ArrayList<SbbsMeBlock>();
				for (int i = 0; i < PAGE_SIZE; i++) {
					tmp.add(Global.listArticle.get(i));
				}
				Global.listArticle.clear();
				Global.listArticle.addAll(tmp);
			}
		}
		lvPullDown.notifyDidLoad();
		if (!SbbsMeAPI.isLogin()) {
			loadUserInfo();
		}

		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_HOME, "");
	}

	private void setIsBottom(boolean bottom) {
		isBottom = bottom;
		if (isBottom) {
			lvPullDown.enableAutoFetchMore(false, 0);
			lvPullDown.showAutoFetchMore(false);
		} else {
			lvPullDown.enableAutoFetchMore(true, 1);
			lvPullDown.showAutoFetchMore(true);
		}
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_main;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		miUser = menu.add(0, MenuIds.MENU_ID_USER, 99, R.string.login);
		miUser.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		miUser.setIcon(android.R.drawable.ic_menu_myplaces);
		miGallery = menu.add(0, MenuIds.MENU_ID_GALLERY, 98, R.string.gallery);
		miGallery.setIcon(android.R.drawable.ic_menu_gallery);
		miGallery.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		miMessage = menu.add(0, MenuIds.MENU_ID_MESSAGE, 97, R.string.message);
		miMessage.setIcon(MiscUtils.loadResIcon(getActivity(),
				R.drawable.ic_menu_notifications));
		miMessage.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

		if (SbbsMeAPI.isLogin()) {
			Message msg = new Message();
			msg.what = 1;
			msg.obj = MiscUtils.loadUserHeadFromFile(Config
					.getHeadPath(getActivity()));
			hSetHead.sendMessage(msg);
		}
	}

	private void setMenuLoginState(boolean login) {
		if (miUser != null) {
			miUser.setEnabled(!login);
		}
	}

	private void loadUserInfo() {
		int type = Config.getAccountType(getActivity());
		switch (type) {
		case 0:
			// google
			String googleUserId = Config.getGoogleUserId(getActivity());
			if (!googleUserId.equals("")) {
				layLogining.setVisibility(View.VISIBLE);
				setMenuLoginState(true);
				googleOAuth.getGoogleUserInfoViaOAuth();
			}
			break;
		case 1:
			// github
			String githubUserId = Config.getGithubUserId(getActivity());
			if (!githubUserId.equals("")) {
				layLogining.setVisibility(View.VISIBLE);
				setMenuLoginState(true);
				githubOAuth.getGithubUserInfoViaOAuth();
			}
			break;
		case 2:
			String sinaUserId = Config.getSinaUserId(getActivity());
			if (!sinaUserId.equals("")) {
				layLogining.setVisibility(View.VISIBLE);
				setMenuLoginState(true);
				sinaOAuth.getSinaUserInfo(sinaUserId);
			}
			break;
		}
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		Global.canExit = false;
		switch (item.getItemId()) {
		case MenuIds.MENU_ID_USER:
			int type = Config.getAccountType(getActivity());
			String userId = "";
			switch (type) {
			case 0:
				// google
				String googleUserId = Config.getGoogleUserId(getActivity());
				userId = googleUserId;
				break;
			case 1:
				// github
				String githubUserId = Config.getGithubUserId(getActivity());
				userId = githubUserId;
				break;
			case 2:
				String sinaUserId = Config.getSinaUserId(getActivity());
				userId = sinaUserId;
				break;
			}

			if (!SbbsMeAPI.isLogin()) {
				startActivityForResult(new Intent(getActivity(),
						SelectLoginDialog.class), 0);
			} else {
				startActivityForResult(new Intent(getActivity(),
						UserDetailActivity.class).putExtra("user", userId), 1);
			}
			break;
		case MenuIds.MENU_ID_GALLERY:
			if (SbbsMeAPI.isLogin()) {
				startActivity(new Intent(getActivity(), GalleryActivity.class)
						.putExtra("select_mode", false));
			} else {
				Toast.makeText(getActivity(), R.string.not_login,
						Toast.LENGTH_LONG).show();
			}
			break;
		case MenuIds.MENU_ID_MESSAGE:
			if (SbbsMeAPI.isLogin()) {
				startActivity(new Intent(getActivity(),
						PrivateMessageActivity.class));
			} else {
				Toast.makeText(getActivity(), R.string.not_login,
						Toast.LENGTH_LONG).show();
			}
			break;
		}
		return true;
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onRefresh() {
		Global.canExit = false;
		page = 1;
		setIsBottom(false);
		loader.setRefresh(true);
		loader.setPage(page, PAGE_SIZE);
		loader.startLoading();
	}

	@Override
	public void onMore() {
		Global.canExit = false;
		if (!isBottom) {
			page++;
			loader.setRefresh(true);
			loader.setPage(page, PAGE_SIZE);
			loader.startLoading();
		} else {
			lvPullDown.notifyDidMore();
		}
	}

	@Override
	public void onLoadComplete(Loader<List<SbbsMeBlock>> loader,
			List<SbbsMeBlock> data) {
		if (page == 1) {
			Global.listArticle.clear();
		}
		if (data != null && data.size() != 0) {
			Global.listArticle.addAll(data);
		} else {
			setIsBottom(true);
		}

		if (getActivity() != null) {
			adapter.setNewList(Global.listArticle);
			lvPullDown.notifyDidRefresh();
			lvPullDown.notifyDidMore();

			if (!((SbbsBlockLoader) loader).isRefresh()) {
				((SbbsBlockLoader) loader).setRefresh(true);
				loader.startLoading();
			} else {
				tvNodata.setEnabled(true);
				tvNodata.setVisibility(Global.listArticle.size() == 0 ? View.VISIBLE
						: View.GONE);
				tvLoading.setVisibility(View.GONE);
			}
		}
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		Global.canExit = false;
		final SbbsMeBlock item = (SbbsMeBlock) lvPullDown.getListView()
				.getItemAtPosition(position);

		startActivityForResult(new Intent(getActivity(), ArticleActivity.class)
				.putExtra("articleId", item.Id).putExtra("item", item), 2);
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode,
			final Intent data) {
		if (resultCode != Activity.RESULT_OK) {
			return;
		}
		switch (requestCode) {
		case 0: {
			int type = data.getIntExtra("type", 0);
			layLogining.setVisibility(View.VISIBLE);
			setMenuLoginState(true);
			switch (type) {
			case 0:
				googleOAuth.sendGoogleOauth();
				break;
			case 1:
				githubOAuth.sendGithubOauth();
				break;
			case 2:
				sinaOAuth.sendSinaOauth();
				break;
			}
		}
			break;
		case 1: {
			int action = data.getIntExtra("action", 0);
			if (action == 1) {
				SbbsMeAPI.logout();
				Config.setAccountType(getActivity(), -1);
				Config.setSinaUserId(getActivity(), "");
				Config.setUserId(getActivity(), "");
				miUser.setIcon(android.R.drawable.ic_menu_myplaces);
				SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_LOGOUT, "");
			}
		}
			break;
		case 2: {
			if (Global.autoRefreshTag) {
				Global.autoRefreshTag = false;
				tvLoading.setVisibility(View.VISIBLE);
				loader.setRefresh(true);
				page = 1;
				loader.setPage(page, PAGE_SIZE);
				loader.startLoading();
			}
		}
			break;
		}
	}

	final Handler hSetHead = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				if (miUser != null) {
					Drawable d = (Drawable) msg.obj;
					if (d != null) {
						miUser.setIcon(d);
					}
				}
				layLogining.setVisibility(View.GONE);
				setMenuLoginState(false);
				SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_LOGIN, "");
			}
			super.handleMessage(msg);
		}
	};

	@Override
	public void onGetSinaUser(final SbbsMeSinaUser user) {
		if (user != null) {
			Drawable d = sinaOAuth.getUserHead(user.avatar_large);
			Message msg = new Message();
			msg.what = 1;
			msg.obj = d;

			try {
				SbbsMeAPI.login(String.valueOf(user.id), user.screen_name,
						"weibo", user.avatar_large);
			} catch (Exception e) {
				Log.e("onGetSinaUser", e.getMessage());
			}
			hSetHead.sendMessage(msg);
		}
	}

	@Override
	public void onGetGoogleUser(SbbsMeGoogleUser user) {
		if (user != null) {
			Drawable d = googleOAuth.getUserHead(user.picture);
			Message msg = new Message();
			msg.what = 1;
			msg.obj = d;

			try {
				SbbsMeAPI.login(user.id, user.name, "google", user.picture);
			} catch (Exception e) {
				Log.e("onGetGoogleUser", e.getMessage());
			}
			hSetHead.sendMessage(msg);
		}
	}

	@Override
	public void onGetGithubUser(SbbsMeGithubUser user) {
		if (user != null) {
			Drawable d = githubOAuth.getUserHead(user.avatarUrl);
			Message msg = new Message();
			msg.what = 1;
			msg.obj = d;

			try {
				SbbsMeAPI.login(String.valueOf(user.id), user.name, "github",
						user.avatarUrl);
			} catch (Exception e) {
				Log.e("onGetGithubUser", e.getMessage());
			}
			hSetHead.sendMessage(msg);
		}
	}

	@Override
	public void onClick(View v) {
		Global.canExit = false;
		switch (v.getId()) {
		case R.id.tvNodata:
			tvNodata.setEnabled(false);
			tvLoading.setVisibility(View.VISIBLE);
			loader.setRefresh(true);
			page = 1;
			loader.setPage(page, PAGE_SIZE);
			loader.startLoading();
			break;
		}
	}

}
