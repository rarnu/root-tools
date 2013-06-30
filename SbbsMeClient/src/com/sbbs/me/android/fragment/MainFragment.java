package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
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
import com.sbbs.me.android.adapter.SbbsMeArticleAdapter;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.consts.MenuIds;
import com.sbbs.me.android.loader.SbbsBlockLoader;

public class MainFragment extends BaseFragment implements
		OnLoadCompleteListener<List<SbbsMeBlock>>, OnPullDownListener,
		OnItemClickListener {

	PullDownListView lvPullDown;
	SbbsBlockLoader loader;
	SbbsMeArticleAdapter adapter;
	TextView tvLoading;
	
	MenuItem miUser;

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

}
