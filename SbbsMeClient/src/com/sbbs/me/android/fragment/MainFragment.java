package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.intf.OnPullDownListener;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeArticleAdapter;
import com.sbbs.me.android.api.SbbsMeArticle;
import com.sbbs.me.android.loader.SbbsArticleLoader;

public class MainFragment extends BaseFragment implements
		OnLoadCompleteListener<List<SbbsMeArticle>>, OnPullDownListener {

	PullDownListView lvPullDown;

	SbbsArticleLoader loader;
	List<SbbsMeArticle> list;
	SbbsMeArticleAdapter adapter;
	TextView tvLoading;

	private Handler hRefresh = new Handler() {

		@Override
		public void handleMessage(Message msg) {
			switch (msg.what) {
			case PullDownListView.WHAT_DID_REFRESH: {
				adapter.notifyDataSetChanged();
				lvPullDown.notifyDidRefresh();
				break;
			}

			case PullDownListView.WHAT_DID_MORE: {
				adapter.notifyDataSetChanged();
				lvPullDown.notifyDidMore();
				break;
			}
			}

		}

	};

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
		list = new ArrayList<SbbsMeArticle>();
		adapter = new SbbsMeArticleAdapter(getActivity(), list);
		lvPullDown.getListView().setAdapter(adapter);
		loader = new SbbsArticleLoader(getActivity());
		lvPullDown.enableAutoFetchMore(false, 1);
		lvPullDown.setOnPullDownListener(this);
		
		lvPullDown.getListView().setDivider(new ColorDrawable(0xFFc5eaf8));
		lvPullDown.getListView().setDividerHeight(UIUtils.dipToPx(1));
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		tvLoading.setVisibility(View.VISIBLE);
		loader.startLoading();
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
		new Thread(new Runnable() {

			@Override
			public void run() {
				try {
					Thread.sleep(500);
				} catch (InterruptedException e) {
				}
				Message msg = new Message();
				msg.what = PullDownListView.WHAT_DID_REFRESH;
				hRefresh.sendMessage(msg);
			}
		}).start();
	}

	@Override
	public void onMore() {
		// new Thread(new Runnable() {
		//
		// @Override
		// public void run() {
		// try {
		// Thread.sleep(1000);
		// } catch (InterruptedException e) {
		// e.printStackTrace();
		// }
		// Message msg = new Message();
		// msg.what = PullDownListView.WHAT_DID_MORE;
		// hRefresh.sendMessage(msg);
		// }
		// }).start();

	}

	@Override
	public void onLoadComplete(Loader<List<SbbsMeArticle>> loader,
			List<SbbsMeArticle> data) {
		list.clear();
		if (data != null) {
			list.addAll(data);
		}
		adapter.setNewList(list);
		tvLoading.setVisibility(View.GONE);
	}

}
