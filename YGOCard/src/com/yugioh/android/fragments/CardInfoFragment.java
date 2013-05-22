package com.yugioh.android.fragments;

import java.io.File;
import java.util.List;

import android.app.Activity;
import android.app.Fragment;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ShareActionProvider;

import com.rarnu.devlib.base.BaseTabFragment;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;
import com.yugioh.android.common.MenuIds;
import com.yugioh.android.define.PathDefine;

public class CardInfoFragment extends BaseTabFragment {

	MenuItem itemShare;

	public CardInfoFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}

	CardInfo info = null;

	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
		info = (CardInfo) getActivity().getIntent().getSerializableExtra(
				"cardinfo");
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
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		itemShare = menu.add(0, MenuIds.MENUID_SHARE, 99, R.string.share);
		itemShare.setIcon(android.R.drawable.ic_menu_share);
		itemShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		ShareActionProvider sap = new ShareActionProvider(getActivity());
		sap.setShareIntent(getShareIntent());
		itemShare.setActionProvider(sap);

	}

	private Intent getShareIntent() {
		Intent shareIntent = new Intent(Intent.ACTION_SEND);
		shareIntent.setType("image/*");
		Uri uri = Uri.fromFile(new File(PathDefine.PICTURE_PATH
				+ String.valueOf(info.getCardID() - 1) + ".jpg"));
		shareIntent.putExtra(Intent.EXTRA_STREAM, uri);
		shareIntent.putExtra(Intent.EXTRA_TEXT, "Share one cadrd");
		return shareIntent;
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public String getCustomTitle() {
		String title = null;
		if (info != null) {
			title = info.getSCCardName();
		}
		return title;
	}

	@Override
	public void initFragmentList(List<Fragment> listFragment) {

		listFragment.add(new CardInfoCardFragment(
				getString(R.tag.tag_card_info),
				getString(R.string.page_cardinfo)));
		listFragment.add(new CardInfoAdjustFragment(
				getString(R.tag.tag_card_adjust),
				getString(R.string.page_cardadjust)));
		listFragment
				.add(new CardInfoPictureFragment(getString(R.tag.tag_card_pic),
						getString(R.string.page_picture)));

	}
	
	@Override
	public Bundle getFragmentState() {
		return null;
	}

}
