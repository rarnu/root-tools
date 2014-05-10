package com.yugioh.android.fragments;

import android.content.Loader;
import android.database.Cursor;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.*;
import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;
import com.yugioh.android.loader.FavLoader;
import com.yugioh.android.utils.MiscUtils;

public class FavFragment extends BaseFragment implements Loader.OnLoadCompleteListener<Cursor>, AdapterView.OnItemClickListener {

    TextView tvListNoCard;
    ListView lvList;
    FavLoader loader;
    Cursor cSearch;
    SimpleCursorAdapter adapterSearch;

    public FavFragment() {
        super();
    }

    @Override
    public int getBarTitle() {
        return R.string.lm_myfav;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.lm_myfav;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        lvList = (ListView) innerView.findViewById(R.id.lvList);
        tvListNoCard = (TextView) innerView.findViewById(R.id.tvListNoCard);
        loader = new FavLoader(getActivity());

    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
        lvList.setOnItemClickListener(this);
    }

    @Override
    public void initLogic() {
    }

    @Override
    public void onResume() {
        super.onResume();
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_myfav;
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
    public void onLoadComplete(Loader<Cursor> loader, Cursor data) {
        if (data != null) {
            cSearch = data;
            adapterSearch = new SimpleCursorAdapter(getActivity(), R.layout.item_card, cSearch, new String[]{"name", "sCardType"}, new int[]{R.id.tvCardName, R.id.tvCardType}, CursorAdapter.FLAG_REGISTER_CONTENT_OBSERVER);
        } else {
            adapterSearch = null;
        }
        if (getActivity() != null) {
            lvList.setAdapter(adapterSearch);
            tvListNoCard.setVisibility((adapterSearch == null || adapterSearch.getCount() == 0) ? View.VISIBLE : View.GONE);
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        MiscUtils.openCardDetail(getActivity(), cSearch, position);
    }
}
