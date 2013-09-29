package com.yugioh.android.fragments;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.*;
import android.widget.AdapterView.OnItemClickListener;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.yugioh.android.MainActivity;
import com.yugioh.android.R;
import com.yugioh.android.intf.IMainIntf;

import java.util.ArrayList;
import java.util.List;

public class LeftMenuFragment extends BaseFragment implements OnItemClickListener {

    ListView lvCard, lvExit;
    ArrayAdapter<String> adapterCard, adapterExit;
    List<String> listCard, listExit;
    ImageView ivLogo;
    TextView tvLeftTitle;
    RelativeLayout.LayoutParams lpLogo = null;

    public LeftMenuFragment() {
        super();
        tagText = ResourceUtils.getString(R.string.tag_menu_left);
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
        lvCard = (ListView) innerView.findViewById(R.id.lvCard);
        lvExit = (ListView) innerView.findViewById(R.id.lvExit);
        tvLeftTitle = (TextView) innerView.findViewById(R.id.tvLeftTitle);
        ivLogo = (ImageView) innerView.findViewById(R.id.ivLogo);

        listCard = new ArrayList<String>();
        listCard.add(getString(R.string.lm_search));        // 0
        listCard.add(getString(R.string.lm_banned));        // 1
        listCard.add(getString(R.string.lm_newcard));       // 2
        listCard.add(getString(R.string.lm_package));       // 3
        listCard.add(getString(R.string.lm_myfav));          // 4
        listCard.add(getString(R.string.lm_tool));          // 5
        listExit = new ArrayList<String>();
        listExit.add(getString(R.string.lm_exit));
        adapterCard = new ArrayAdapter<String>(getActivity(), R.layout.item_menu, listCard);
        adapterExit = new ArrayAdapter<String>(getActivity(), R.layout.item_menu, listExit);
        lvCard.setAdapter(adapterCard);
        lvExit.setAdapter(adapterExit);

    }

    @Override
    public void initEvents() {
        lvCard.setOnItemClickListener(this);
        lvExit.setOnItemClickListener(this);
    }

    @Override
    public void initLogic() {

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.menu_left;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position,
                            long id) {
        switch (parent.getId()) {
            case R.id.lvCard:
                // switch page
                ((IMainIntf) getActivity()).switchPage(position, true);
                break;
            case R.id.lvExit:
                getActivity().finish();
                break;
        }

    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
