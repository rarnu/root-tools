package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.egit.github.core.Repository;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.intf.OnPullDownListener;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.Global;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeGithubAdapter;
import com.sbbs.me.android.loader.SbbsGithubLoader;

public class OnGithubFragment extends BaseFragment implements
		OnLoadCompleteListener<List<Repository>>, OnPullDownListener,
		OnItemClickListener {

	PullDownListView repoPullDown;
	SbbsGithubLoader loader;
	SbbsMeGithubAdapter adapter;
	TextView repoLoading;
	int type = 0;

	public OnGithubFragment(int type) {
		super();
		this.type = type;
		tagText = ResourceUtils
				.getString(type == 0 ? R.tag.tag_ongithub_fragment_sbbs
						: R.tag.tag_ongithub_fragment_android);
		tabTitle = ResourceUtils.getString(type == 0 ? R.string.project_sbbs_me
				: R.string.project_android);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_ongithub;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_ongithub;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {

		repoPullDown = (PullDownListView) innerView
				.findViewById(R.id.repoPullDown);
		repoLoading = (TextView) innerView.findViewById(R.id.repoLoading);
		if (Global.listRepos == null) {
			Global.listRepos = new ArrayList<Repository>();
		}
		adapter = new SbbsMeGithubAdapter(getActivity(), Global.listRepos);
		repoPullDown.getListView().setAdapter(adapter);
		loader = new SbbsGithubLoader(getActivity());
		repoPullDown.enableAutoFetchMore(true, 1);
		repoPullDown.setOnPullDownListener(this);
		repoPullDown.getListView().setDivider(new ColorDrawable(0xFFc5eaf8));
		repoPullDown.getListView().setDividerHeight(UIUtils.dipToPx(1));
	}

	@Override
	public void initEvents() {
		repoPullDown.getListView().setOnItemClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {

		if (Global.listRepos.size() == 0) {
			repoLoading.setVisibility(View.VISIBLE);
			loader.startLoading();
		}
		repoPullDown.notifyDidLoad();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_ongithub;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public void onLoadComplete(Loader<List<Repository>> loader,
			List<Repository> data) {
		Global.listRepos.clear();
		if (data != null) {
			Global.listRepos.addAll(data);
		}
		adapter.setNewList(Global.listRepos);
		repoLoading.setVisibility(View.GONE);
		repoPullDown.notifyDidRefresh();
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
				repoPullDown.notifyDidMore();
			}
			super.handleMessage(msg);
		};
	};

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {/*
					 * final SbbsMeGithub item = (SbbsMeGithub)
					 * repoPullDown.getListView() .getItemAtPosition(position);
					 * 
					 * startActivity(new Intent(getActivity(),
					 * ArticleActivity.class) .putExtra());
					 */
	}
}
