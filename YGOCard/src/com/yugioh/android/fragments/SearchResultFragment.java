package com.yugioh.android.fragments;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.database.Cursor;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.CursorAdapter;
import android.widget.ListView;
import android.widget.SimpleCursorAdapter;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.base.BaseTabFragment;
import com.yugioh.android.AboutActivity;
import com.yugioh.android.CardInfoActivity;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;
import com.yugioh.android.database.YugiohUtils;
import com.yugioh.android.define.FieldDefine;
import com.yugioh.android.loader.SearchLoader;
import com.yugioh.android.utils.ResourceUtils;

public class SearchResultFragment extends BaseFragment implements
		OnItemClickListener, OnLoadCompleteListener<Cursor> {

	Cursor cSearchResult;
	SimpleCursorAdapter adapterSearchResult;

	ListView lvList;
	TextView tvListNoCard;

	SearchLoader loaderSearch;

	public SearchResultFragment() {
		super();
		tagText = ResourceUtils.getString(R.string.tag_main_result);
		tabTitle = ResourceUtils.getString(R.string.page_list);
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
	public void initComponents() {
		lvList = (ListView) innerView.findViewById(R.id.lvList);
		tvListNoCard = (TextView) innerView.findViewById(R.id.tvListNoCard);
	}

	@Override
	public void initEvents() {
		lvList.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_search_result;
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
		tvListNoCard.setText(R.string.list_nocard_searching);
		BaseTabFragment btf = (BaseTabFragment) getFragmentManager()
				.findFragmentByTag(getString(R.string.tag_main));
		btf.setTabPosition(1);

		loaderSearch = new SearchLoader(getActivity(), bn);
		loaderSearch.registerListener(0, this);
		loaderSearch.startLoading();

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		cSearchResult.moveToPosition(position);
		int cardId = cSearchResult.getInt(0);
		if (cardId == 1) {
			startActivity(new Intent(getActivity(), AboutActivity.class));
		} else {
			Intent inCardInfo = new Intent(getActivity(),
					CardInfoActivity.class);
			CardInfo info = YugiohUtils.getOneCard(getActivity(), cardId);
			inCardInfo.putExtra("cardinfo", info);
			startActivity(inCardInfo);
		}
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void onLoadComplete(Loader<Cursor> loader, Cursor data) {
		if (data != null) {
			cSearchResult = data;
			adapterSearchResult = new SimpleCursorAdapter(getActivity(),
					R.layout.item_card, cSearchResult, new String[] {
							FieldDefine.DataFields[5],
							FieldDefine.DataFields[10] }, new int[] {
							R.id.tvCardName, R.id.tvCardType },
					CursorAdapter.FLAG_REGISTER_CONTENT_OBSERVER);
			lvList.setAdapter(adapterSearchResult);
			tvListNoCard
					.setVisibility(adapterSearchResult.getCount() == 0 ? View.VISIBLE
							: View.GONE);
			tvListNoCard.setText(R.string.list_nocard);

		}

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

}
