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
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.intf.OnPullDownListener;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.ArticleActivity;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.SelectLoginActivity;
import com.sbbs.me.android.UserDetailActivity;
import com.sbbs.me.android.adapter.SbbsMeArticleAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.api.SbbsMeGoogleUser;
import com.sbbs.me.android.api.SbbsMeSinaUser;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.loader.SbbsBlockLoader;
import com.sbbs.me.android.utils.Config;
import com.sbbs.me.android.utils.GoogleOAuth;
import com.sbbs.me.android.utils.GoogleOAuth.GoogleUserCallback;
import com.sbbs.me.android.utils.SinaOAuth;
import com.sbbs.me.android.utils.SinaOAuth.SinaUserCallback;

public class MainFragment extends BaseFragment implements
		OnLoadCompleteListener<List<SbbsMeBlock>>, OnPullDownListener,
		OnItemClickListener, SinaUserCallback, GoogleUserCallback {

	PullDownListView lvPullDown;
	SbbsBlockLoader loader;
	SbbsMeArticleAdapter adapter;
	TextView tvLoading;

	MenuItem miUser;
	SinaOAuth sinaOAuth;
	GoogleOAuth googleOAuth;

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
		if (Global.listArticle == null) {
			Global.listArticle = new ArrayList<SbbsMeBlock>();
		}
		adapter = new SbbsMeArticleAdapter(getActivity(), Global.listArticle);
		lvPullDown.getListView().setAdapter(adapter);
		loader = new SbbsBlockLoader(getActivity());
		lvPullDown.enableAutoFetchMore(true, 1);
		lvPullDown.setOnPullDownListener(this);

		int devide = UIUtils.dipToPx(8);
		lvPullDown.getListView().setDivider(null);
		lvPullDown.getListView().setDividerHeight(devide);

		lvPullDown.getListView().setPadding(devide, devide, devide, devide);

		sinaOAuth = new SinaOAuth(getActivity(), this);
		googleOAuth = new GoogleOAuth(getActivity(), this);
	}

	@Override
	public void initEvents() {
		lvPullDown.getListView().setOnItemClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {

		if (Global.listArticle.size() == 0) {
			tvLoading.setVisibility(View.VISIBLE);
			loader.startLoading();
		}
		lvPullDown.notifyDidLoad();
		loadUserInfo();
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
		miUser.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		miUser.setIcon(android.R.drawable.ic_menu_report_image);
	}

	private void loadUserInfo() {
		int type = Config.getAccountType(getActivity());
		switch (type) {
		case 0:
			// google
			String googleUserId = Config.getGoogleUserId(getActivity());
			if (!googleUserId.equals("")) {
				googleOAuth.getGoogleUserInfoViaOAuth();
			}
			break;
		case 1:
			// github
			break;
		case 2:
			String sinaUserId = Config.getSinaUserId(getActivity());
			if (!sinaUserId.equals("")) {
				sinaOAuth.getSinaUserInfo(sinaUserId);
			}
			break;
		}
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
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
				break;
			case 2:
				String sinaUserId = Config.getSinaUserId(getActivity());
				userId = sinaUserId;
				break;
			}
			if (userId.equals("")) {
				startActivityForResult(new Intent(getActivity(),
						SelectLoginActivity.class), 0);
			} else {
				startActivityForResult(new Intent(getActivity(),
						UserDetailActivity.class), 1);
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
		loader.startLoading();
	}

	@Override
	public void onMore() {
		new Thread(new Runnable() {

			@Override
			public void run() {
				try {
					Thread.sleep(500);
				} catch (Exception e) {

				}
				hDid.sendEmptyMessage(1);
			}
		}).start();

	}

	private Handler hDid = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				lvPullDown.notifyDidMore();
			}
			super.handleMessage(msg);
		};
	};

	@Override
	public void onLoadComplete(Loader<List<SbbsMeBlock>> loader,
			List<SbbsMeBlock> data) {
		Global.listArticle.clear();
		if (data != null) {
			Global.listArticle.addAll(data);
		}

		adapter.setNewList(Global.listArticle);
		tvLoading.setVisibility(View.GONE);
		lvPullDown.notifyDidRefresh();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		final SbbsMeBlock item = (SbbsMeBlock) lvPullDown.getListView()
				.getItemAtPosition(position);

		startActivity(new Intent(getActivity(), ArticleActivity.class)
				.putExtra("articleId", item.Id));
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
			switch (type) {
			case 0:
				// google
				googleOAuth.sendGoogleOauth();
				break;
			case 1:
				// github
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
				Config.setAccountType(getActivity(), -1);
				Config.setSinaUserId(getActivity(), "");
				Config.setUserId(getActivity(), "");
				miUser.setIcon(android.R.drawable.ic_menu_report_image);
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
			hSetHead.sendMessage(msg);
			try {
				SbbsMeAPI.login(String.valueOf(user.id), user.screen_name, "weibo", user.avatar_large);
			} catch (Exception e) {
				Log.e("onGetSinaUser", e.getMessage());
			}
		} else {
			sinaOAuth.sendSinaOauth();
		}
	}

	@Override
	public void onGetGoogleUser(SbbsMeGoogleUser user) {
		if (user != null) {
			Drawable d = googleOAuth.getUserHead(user.picture);
			Message msg = new Message();
			msg.what = 1;
			msg.obj = d;
			hSetHead.sendMessage(msg);
			try {
				SbbsMeAPI.login(user.id, user.name, "google", user.picture);
			} catch (Exception e) {
				Log.e("onGetGoogleUser", e.getMessage());
			}
		}
	}

}
