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
import com.yugioh.android.loader.NewCardLoader;
import com.yugioh.android.utils.MiscUtils;

public class NewCardFragment extends BaseFragment implements OnItemClickListener, OnLoadCompleteListener<Cursor> {

    ListView lvNewCard;
    Cursor cNewCard;
    SimpleCursorAdapter adapterNewCard;
    NewCardLoader loaderNewcard;

    public NewCardFragment() {
        super();
    }

    @Override
    public int getBarTitle() {
        return R.string.lm_newcard;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.lm_newcard;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_newcard;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initComponents() {
        lvNewCard = (ListView) innerView.findViewById(R.id.lvNewCard);
        loaderNewcard = new NewCardLoader(getActivity());
    }

    @Override
    public void initEvents() {
        lvNewCard.setOnItemClickListener(this);
        loaderNewcard.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        loaderNewcard.startLoading();

    }

    @Override
    public void initMenu(Menu arg0) {

    }

    @Override
    public void onGetNewArguments(Bundle arg0) {

    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        MiscUtils.openCardDetail(getActivity(), cNewCard, position);
    }

    @Override
    public void onLoadComplete(Loader<Cursor> loader, Cursor data) {
        if (data != null) {
            cNewCard = data;
            adapterNewCard = new SimpleCursorAdapter(getActivity(), R.layout.item_card, cNewCard, new String[]{"name", "sCardType"}, new int[]{R.id.tvCardName, R.id.tvCardType}, CursorAdapter.FLAG_REGISTER_CONTENT_OBSERVER);

        }
        if (getActivity() != null) {
            lvNewCard.setAdapter(adapterNewCard);
        }
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
