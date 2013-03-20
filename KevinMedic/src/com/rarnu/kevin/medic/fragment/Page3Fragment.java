package com.rarnu.kevin.medic.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.PullDownListView;
import com.rarnu.devlib.component.event.OnPullDownListener;
import com.rarnu.kevin.medic.MainActivity;
import com.rarnu.kevin.medic.R;
import com.rarnu.kevin.medic.adapter.DataItemAdapter;
import com.rarnu.kevin.medic.loader.DataLoader;

public class Page3Fragment extends BaseFragment implements OnLoadCompleteListener<List<String>>, OnPullDownListener {

	PullDownListView lvData;
	List<String> lstData;
	DataItemAdapter adapterData;
	
	DataLoader loader;
	
	private Handler hRefresh = new Handler() {

		@Override
		public void handleMessage(Message msg) {
			switch (msg.what) {
			case PullDownListView.WHAT_DID_REFRESH: {
				lstData.add(0, "Add on Top");
				adapterData.notifyDataSetChanged();
				lvData.notifyDidRefresh();
				break;
			}

			case PullDownListView.WHAT_DID_MORE: {
				lstData.add("Add on Bottom");
				adapterData.notifyDataSetChanged();
				lvData.notifyDidMore();
				break;
			}
			}

		}

	};
	
	@Override
	protected int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	protected void initComponents() {
		lvData = (PullDownListView) innerView.findViewById(R.id.lvData);
		lstData = new ArrayList<String>();
		adapterData = new DataItemAdapter(getActivity(), lstData);
		lvData.getListView().setAdapter(adapterData);
		loader = new DataLoader(getActivity(), 3);
		lvData.enableAutoFetchMore(true, 1);
		lvData.setOnPullDownListener(this);
	}

	@Override
	protected void initEvents() {
		loader.registerListener(0, this);
		
	}

	@Override
	protected void initLogic() {
		loader.startLoading();
		lvData.notifyDidLoad();
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_page_2;
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void initMenu(Menu menu) {


	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public void onLoadComplete(Loader<List<String>> loader, List<String> data) {
		lstData.clear();
		if (data != null) {
			lstData.addAll(data);
		}
		adapterData.setNewList(lstData);
		
	}

	@Override
	public void onRefresh() {
		new Thread(new Runnable() {

			@Override
			public void run() {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
				Message msg = new Message();
				msg.what = PullDownListView.WHAT_DID_REFRESH;
				hRefresh.sendMessage(msg);
			}
		}).start();
		
	}

	@Override
	public void onMore() {
		new Thread(new Runnable() {

			@Override
			public void run() {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
				Message msg = new Message();
				msg.what = PullDownListView.WHAT_DID_MORE;
				hRefresh.sendMessage(msg);
			}
		}).start();
		
	}

}
