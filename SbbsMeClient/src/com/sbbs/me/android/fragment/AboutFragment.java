package com.sbbs.me.android.fragment;

import java.util.List;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.DeviceUtilsLite;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.FeedbackActivity;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeWeiboAdapter;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeLogs;
import com.sbbs.me.android.api.SbbsMeWeibo;

public class AboutFragment extends BaseFragment implements OnClickListener {

	Button btnFeedback;
	TextView tvVersion;
	ListView lvWeibo;
	SbbsMeWeiboAdapter adapter;
	List<SbbsMeWeibo> list;

	public AboutFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_about_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_about;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_about;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	private Handler hClick = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				String url = (String) msg.obj;
				if (url != null && !url.equals("")) {
					Intent inOpenWebsite = new Intent(Intent.ACTION_VIEW);
					inOpenWebsite.setData(Uri.parse(url));
					startActivity(inOpenWebsite);
					SbbsMeAPI.writeLogT(getActivity(),
							SbbsMeLogs.LOG_ABOUT_WEIBO, url);
				}
			}
			super.handleMessage(msg);
		};
	};

	@Override
	public void initComponents() {
		btnFeedback = (Button) innerView.findViewById(R.id.btnFeedback);
		tvVersion = (TextView) innerView.findViewById(R.id.tvVersion);
		lvWeibo = (ListView) innerView.findViewById(R.id.lvWeibo);

		list = SbbsMeAPI.getCredit(getActivity());
		adapter = new SbbsMeWeiboAdapter(getActivity(), list, hClick);
		lvWeibo.setAdapter(adapter);
	}

	@Override
	public void initEvents() {
		btnFeedback.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		tvVersion.setText(getString(R.string.app_version_about,
				DeviceUtilsLite.getAppVersionName(getActivity())));

		SbbsMeAPI.writeLogT(getActivity(), SbbsMeLogs.LOG_ABOUT, "");

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_about;
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
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnFeedback:
			startActivity(new Intent(getActivity(), FeedbackActivity.class));
			break;
		}
	}

}
