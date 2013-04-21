package com.yugioh.android;

import android.app.Fragment;
import android.content.Context;

import com.yugioh.android.fragments.CardInfoAdjustFragment;
import com.yugioh.android.fragments.CardInfoCardFragment;
import com.yugioh.android.fragments.CardInfoFragment;
import com.yugioh.android.fragments.CardInfoPictureFragment;
import com.yugioh.android.fragments.LeftMenuFragment;
import com.yugioh.android.fragments.MainFragment;
import com.yugioh.android.fragments.RightMenuFragment;
import com.yugioh.android.fragments.SearchFragment;
import com.yugioh.android.fragments.SearchResultFragment;

public class Fragments {

	private static MainFragment fMain = null;
	private static SearchFragment fSearch = null;
	private static SearchResultFragment fSearchResult = null;
	private static LeftMenuFragment fLeftMenu = null;
	private static RightMenuFragment fRightMenu = null;
	private static CardInfoFragment fCardInfo = null;
	private static CardInfoCardFragment fCardInfoCard = null;
	private static CardInfoAdjustFragment fCardInfoAdjust = null;
	private static CardInfoPictureFragment fCardInfoPicture = null;

	public static void Load(Context context) {
		fMain = new MainFragment();
		fSearch = new SearchFragment(context.getString(R.string.page_search),
				context.getString(R.string.page_search));
		fSearchResult = new SearchResultFragment(
				context.getString(R.string.page_list),
				context.getString(R.string.page_list));
		fLeftMenu = new LeftMenuFragment();
		fRightMenu = new RightMenuFragment();
		fCardInfo = new CardInfoFragment();
		fCardInfoCard = new CardInfoCardFragment(
				context.getString(R.string.page_cardinfo),
				context.getString(R.string.page_cardinfo));
		fCardInfoAdjust = new CardInfoAdjustFragment(
				context.getString(R.string.page_cardadjust),
				context.getString(R.string.page_cardadjust));
		fCardInfoPicture = new CardInfoPictureFragment(
				context.getString(R.string.page_picture),
				context.getString(R.string.page_picture));
	}

	public static void Release() {
		fMain = null;
		fSearch = null;
		fSearchResult = null;
		fLeftMenu = null;
		fRightMenu = null;
		fCardInfo = null;
		fCardInfoCard = null;
		fCardInfoAdjust = null;
		fCardInfoPicture = null;
	}

	public static Fragment getFragment(Context context, String name) {
		Fragment f = null;
		if (name.equals(FragmentNames.FRAGMENT_MAIN)) {
			if (fMain == null) {
				fMain = new MainFragment();
			}
			f = fMain;
		} else if (name.equals(FragmentNames.FRAGMENT_SEARCH)) {
			if (fSearch == null) {
				fSearch = new SearchFragment(
						context.getString(R.string.page_search),
						context.getString(R.string.page_search));
			}
			f = fSearch;
		} else if (name.equals(FragmentNames.FRAGMENT_SEARCH_RESULT)) {
			if (fSearchResult == null) {
				fSearchResult = new SearchResultFragment(
						context.getString(R.string.page_list),
						context.getString(R.string.page_list));
			}
			f = fSearchResult;
		} else if (name.equals(FragmentNames.FRAGMENT_LEFTMENU)) {
			if (fLeftMenu == null) {
				fLeftMenu = new LeftMenuFragment();
			}
			f = fLeftMenu;
		} else if (name.equals(FragmentNames.FRAGMENT_RIGHTMENU)) {
			if (fRightMenu == null) {
				fRightMenu = new RightMenuFragment();
			}
			f = fRightMenu;
		} else if (name.equals(FragmentNames.FRAGMENT_CARDINFO)) {
			if (fCardInfo == null) {
				fCardInfo = new CardInfoFragment();
			}
			f = fCardInfo;
		} else if (name.equals(FragmentNames.FRAGMENT_CARDINFO_CARD)) {
			if (fCardInfoCard == null) {
				fCardInfoCard = new CardInfoCardFragment(
						context.getString(R.string.page_cardinfo),
						context.getString(R.string.page_cardinfo));
			}
			f = fCardInfoCard;
		} else if (name.equals(FragmentNames.FRAGMENT_CARDINFO_ADJUST)) {
			if (fCardInfoAdjust == null) {
				fCardInfoAdjust = new CardInfoAdjustFragment(
						context.getString(R.string.page_cardadjust),
						context.getString(R.string.page_cardadjust));
			}
			f = fCardInfoAdjust;
		} else if (name.equals(FragmentNames.FRAGMENT_CARDINFO_PICTURE)) {
			if (fCardInfoPicture == null) {
				fCardInfoPicture = new CardInfoPictureFragment(
						context.getString(R.string.page_picture),
						context.getString(R.string.page_picture));
			}
			f = fCardInfoPicture;
		}

		return f;
	}
}
