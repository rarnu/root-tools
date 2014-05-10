package com.yugioh.android.fragments;

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
import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;
import com.yugioh.android.loader.LimitLoader;
import com.yugioh.android.utils.MiscUtils;

public class LimitDetailFragment extends BaseFragment implements OnItemClickListener, OnLoadCompleteListener<Cursor> {

    protected int detailType;
    ListView lvLimitCard;
    Cursor cLimit;
    SimpleCursorAdapter adapterLimit;
    LimitLoader loaderLimit;

    public LimitDetailFragment() {
        super();
    }

    @Override
    public int getBarTitle() {
        return R.string.lm_banned;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.lm_banned;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_limit_detail;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initComponents() {
        lvLimitCard = (ListView) innerView.findViewById(R.id.lvLimitCard);
        loaderLimit = new LimitLoader(getActivity(), detailType);
    }

    @Override
    public void initEvents() {
        lvLimitCard.setOnItemClickListener(this);
        loaderLimit.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        loaderLimit.startLoading();
    }

    @Override
    public void initMenu(Menu arg0) {

    }

    @Override
    public void onGetNewArguments(Bundle arg0) {

    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        MiscUtils.openCardDetail(getActivity(), cLimit, position);

    }

    @Override
    public void onLoadComplete(Loader<Cursor> loader, Cursor data) {
        if (data != null) {
            cLimit = data;
            adapterLimit = new SimpleCursorAdapter(getActivity(), R.layout.item_card, cLimit, new String[]{"name", "sCardType"}, new int[]{R.id.tvCardName, R.id.tvCardType}, CursorAdapter.FLAG_REGISTER_CONTENT_OBSERVER);

        }
        if (getActivity() != null) {
            lvLimitCard.setAdapter(adapterLimit);
        }
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
