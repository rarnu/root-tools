package com.rarnu.adcenter.fragment;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings.LayoutAlgorithm;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.Button;

import com.rarnu.adcenter.AnswerActivity;
import com.rarnu.adcenter.R;
import com.rarnu.adcenter.classes.AdItem;
import com.rarnu.adcenter.classes.QuestItem;
import com.rarnu.adcenter.common.MenuIds;
import com.rarnu.adcenter.database.AdUtils;
import com.rarnu.adcenter.loader.QuestLoader;
import com.rarnu.adcenter.utils.MiscUtils;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;

public class AdDetailFragment extends BaseFragment implements OnClickListener,
		OnLoadCompleteListener<QuestItem> {

	AdItem item = null;
	Button btnWebsite;
	WebView wv;
	QuestLoader loader;
	QuestItem quest;
	MenuItem itemCash;

	public AdDetailFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_ad_detail);
	}

	@Override
	public int getBarTitle() {
		return 0;
	}

	@Override
	public int getBarTitleWithPath() {
		return 0;
	}

	@Override
	public String getCustomTitle() {
		String ret = "";
		if (item != null) {
			ret = item.title;
		}
		return ret;
	}

	@Override
	public void initComponents() {
		btnWebsite = (Button) innerView.findViewById(R.id.btnWebsite);
		wv = (WebView) innerView.findViewById(R.id.wv);
		wv.getSettings().setJavaScriptEnabled(true);
		wv.getSettings().setLayoutAlgorithm(LayoutAlgorithm.SINGLE_COLUMN);
		wv.setWebChromeClient(new WebChromeClient() {

		});
		wv.setWebViewClient(new WebViewClient() {
			@Override
			public boolean shouldOverrideUrlLoading(WebView view, String url) {
				wv.loadUrl(url);
				return true;
			}
		});

		loader = new QuestLoader(getActivity());
	}

	@Override
	public void initEvents() {
		btnWebsite.setOnClickListener(this);
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		item = (AdItem) getArguments().getSerializable("item");
		wv.post(new Runnable() {

			@Override
			public void run() {
				wv.loadUrl(item.click_url);

			}
		});
		loader.startLoading();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_ad_detail;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		itemCash = menu.add(0, MenuIds.MENUID_CASH, 99, R.string.my_cash);
		itemCash.setIcon(MiscUtils.loadResIcon(getActivity(), R.drawable.money_bag));
		itemCash.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
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
		case R.id.btnWebsite:
			if (quest != null) {
				Intent inAnswer = new Intent(getActivity(),
						AnswerActivity.class);
				inAnswer.putExtra("item", quest);
				startActivityForResult(inAnswer, 0);
			}
			break;
		}

	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != Activity.RESULT_OK) {
			return;
		}

		switch (requestCode) {
		case 0:
			AdUtils.setAdQuested(getActivity(), item.id);
			btnWebsite.setVisibility(View.GONE);
			break;
		}
	}

	@Override
	public void onLoadComplete(Loader<QuestItem> loader, QuestItem data) {
		quest = data;
		if (data != null) {
			if (getActivity() != null) {
				boolean quested = AdUtils.getAdQuested(getActivity(), item.id);
				Log.e("quested", quested ? "TRUE" : "FALSE");
				btnWebsite.setVisibility(quested ? View.GONE : View.VISIBLE);
				btnWebsite.setText(data.quest);
			}
		}

	}

}
