package com.yugioh.android.fragments;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ConfigUtils;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;
import com.yugioh.android.CoinDiceActivity;
import com.yugioh.android.R;
import com.yugioh.android.common.MenuIds;

public class DuelToolFragment extends BaseFragment implements OnClickListener {

    private static final String KEY_P1LIFE = "key_p1_life";
    private static final String KEY_P2LIFE = "key_p2_life";
    TextView tvPlayer1Life, tvPlayer2Life;
    EditText etP1Life, etP2Life;
    Button btnP1Add, btnP2Add, btnP1Minus, btnP2Minus, btnP1Set, btnP2Set;
    Button btnP1Half, btnP2Half, btnP1Double, btnP2Double, btnP1Divide, btnP2Divide, btnP1Opt, btnP2Opt;
    Button btnCoin, btnDice;
    MenuItem itemResetDuel;
    private int Player1Life = 8000, Player2Life = 8000;

    public DuelToolFragment() {
        super();
    }

    @Override
    public int getBarTitle() {
        return R.string.lm_tool;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.lm_tool;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_tool;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initComponents() {
        tvPlayer1Life = (TextView) innerView.findViewById(R.id.tvPlayer1Life);
        tvPlayer2Life = (TextView) innerView.findViewById(R.id.tvPlayer2Life);
        etP1Life = (EditText) innerView.findViewById(R.id.etP1Life);
        etP2Life = (EditText) innerView.findViewById(R.id.etP2Life);
        btnP1Add = (Button) innerView.findViewById(R.id.btnP1Add);
        btnP2Add = (Button) innerView.findViewById(R.id.btnP2Add);
        btnP1Minus = (Button) innerView.findViewById(R.id.btnP1Minus);
        btnP2Minus = (Button) innerView.findViewById(R.id.btnP2Minus);
        btnP1Set = (Button) innerView.findViewById(R.id.btnP1Set);
        btnP2Set = (Button) innerView.findViewById(R.id.btnP2Set);

        btnP1Half = (Button) innerView.findViewById(R.id.btnP1Half);
        btnP2Half = (Button) innerView.findViewById(R.id.btnP2Half);
        btnP1Double = (Button) innerView.findViewById(R.id.btnP1Double);
        btnP2Double = (Button) innerView.findViewById(R.id.btnP2Double);
        btnP1Divide = (Button) innerView.findViewById(R.id.btnP1Divide);
        btnP2Divide = (Button) innerView.findViewById(R.id.btnP2Divide);
        btnP1Opt = (Button) innerView.findViewById(R.id.btnP1Opt);
        btnP2Opt = (Button) innerView.findViewById(R.id.btnP2Opt);

        btnCoin = (Button) innerView.findViewById(R.id.btnCoin);
        btnDice = (Button) innerView.findViewById(R.id.btnDice);

        setToolButtonLayout();

    }

    @Override
    public void initEvents() {
        btnP1Add.setOnClickListener(this);
        btnP2Add.setOnClickListener(this);
        btnP1Minus.setOnClickListener(this);
        btnP2Minus.setOnClickListener(this);
        btnP1Set.setOnClickListener(this);
        btnP2Set.setOnClickListener(this);

        btnP1Half.setOnClickListener(this);
        btnP2Half.setOnClickListener(this);
        btnP1Double.setOnClickListener(this);
        btnP2Double.setOnClickListener(this);
        btnP1Divide.setOnClickListener(this);
        btnP2Divide.setOnClickListener(this);
        btnP1Opt.setOnClickListener(this);
        btnP2Opt.setOnClickListener(this);

        btnCoin.setOnClickListener(this);
        btnDice.setOnClickListener(this);
    }

    private void setToolButtonLayout() {

        int width = (UIUtils.getWidth() - UIUtils.dipToPx(16)) / 4;

        UIUtils.setViewSizeX(btnP1Half, width);
        UIUtils.setViewSizeX(btnP2Half, width);
        UIUtils.setViewSizeX(btnP1Double, width);
        UIUtils.setViewSizeX(btnP2Double, width);
        UIUtils.setViewSizeX(btnP1Divide, width);
        UIUtils.setViewSizeX(btnP2Divide, width);
        UIUtils.setViewSizeX(btnP1Opt, width);
        UIUtils.setViewSizeX(btnP2Opt, width);

        width = (UIUtils.getWidth() - UIUtils.dipToPx(16)) / 2;
        UIUtils.setViewSizeX(btnCoin, width);
        UIUtils.setViewSizeX(btnDice, width);

    }

    @Override
    public void initLogic() {
        restoreLifePoints();
    }

    @Override
    public void initMenu(Menu menu) {
        itemResetDuel = menu.add(0, MenuIds.MENUID_RESET_DUEL, 99, R.string.tool_reset);
        itemResetDuel.setIcon(android.R.drawable.ic_menu_revert);
        itemResetDuel.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuIds.MENUID_RESET_DUEL:
                Player1Life = 8000;
                Player2Life = 8000;
                setLifePoint(tvPlayer1Life, Player1Life);
                setLifePoint(tvPlayer2Life, Player2Life);
                break;
        }
        return true;
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public void onPause() {
        backupLifePoints();
        super.onPause();
    }

    @Override
    public void onResume() {
        super.onResume();
        restoreLifePoints();
    }

    private void backupLifePoints() {
        ConfigUtils.setIntConfig(getActivity(), KEY_P1LIFE, Player1Life);
        ConfigUtils.setIntConfig(getActivity(), KEY_P2LIFE, Player2Life);
    }

    private void restoreLifePoints() {
        Player1Life = ConfigUtils.getIntConfig(getActivity(), KEY_P1LIFE, 8000);
        Player2Life = ConfigUtils.getIntConfig(getActivity(), KEY_P2LIFE, 8000);
        setLifePoint(tvPlayer1Life, Player1Life);
        setLifePoint(tvPlayer2Life, Player2Life);
    }

    @Override
    public void onClick(View v) {
        int point = 0;
        int life = 0;

        switch (v.getId()) {
            case R.id.btnP1Add:
                if (etP1Life.getText().toString().equals("")) {
                    Toast.makeText(getActivity(), R.string.tool_number_reqired, Toast.LENGTH_LONG).show();
                    return;
                }
                point = Integer.parseInt(etP1Life.getText().toString());
                Player1Life += point;
                setLifePoint(tvPlayer1Life, Player1Life);
                etP1Life.setText("");
                break;
            case R.id.btnP2Add:
                if (etP2Life.getText().toString().equals("")) {
                    Toast.makeText(getActivity(), R.string.tool_number_reqired, Toast.LENGTH_LONG).show();
                    return;
                }
                point = Integer.parseInt(etP2Life.getText().toString());
                Player2Life += point;
                setLifePoint(tvPlayer2Life, Player2Life);
                etP2Life.setText("");
                break;
            case R.id.btnP1Minus:
                if (etP1Life.getText().toString().equals("")) {
                    Toast.makeText(getActivity(), R.string.tool_number_reqired, Toast.LENGTH_LONG).show();
                    return;
                }
                point = Integer.parseInt(etP1Life.getText().toString());
                Player1Life -= point;
                if (Player1Life < 0) {
                    Player1Life = 0;
                }
                setLifePoint(tvPlayer1Life, Player1Life);
                etP1Life.setText("");
                break;
            case R.id.btnP2Minus:
                if (etP2Life.getText().toString().equals("")) {
                    Toast.makeText(getActivity(), R.string.tool_number_reqired, Toast.LENGTH_LONG).show();
                    return;
                }
                point = Integer.parseInt(etP2Life.getText().toString());
                Player2Life -= point;
                if (Player2Life < 0) {
                    Player2Life = 0;
                }
                setLifePoint(tvPlayer2Life, Player2Life);
                etP2Life.setText("");
                break;
            case R.id.btnP1Set:
                if (etP1Life.getText().toString().equals("")) {
                    Toast.makeText(getActivity(), R.string.tool_number_reqired, Toast.LENGTH_LONG).show();
                    return;
                }
                point = Integer.parseInt(etP1Life.getText().toString());
                Player1Life = point;
                setLifePoint(tvPlayer1Life, Player1Life);
                etP1Life.setText("");
                break;
            case R.id.btnP2Set:
                if (etP2Life.getText().toString().equals("")) {
                    Toast.makeText(getActivity(), R.string.tool_number_reqired, Toast.LENGTH_LONG).show();
                    return;
                }
                point = Integer.parseInt(etP2Life.getText().toString());
                Player2Life = point;
                setLifePoint(tvPlayer2Life, Player2Life);
                etP2Life.setText("");
                break;

            case R.id.btnP1Half:
                if (Player1Life % 2 != 0) {
                    Player1Life += 1;
                }
                Player1Life /= 2;
                setLifePoint(tvPlayer1Life, Player1Life);
                break;

            case R.id.btnP2Half:
                if (Player2Life % 2 != 0) {
                    Player2Life += 1;
                }
                Player2Life /= 2;
                setLifePoint(tvPlayer2Life, Player2Life);
                break;

            case R.id.btnP1Double:
                Player1Life *= 2;
                setLifePoint(tvPlayer1Life, Player1Life);
                break;

            case R.id.btnP2Double:
                Player2Life *= 2;
                setLifePoint(tvPlayer2Life, Player2Life);
                break;
            case R.id.btnP1Divide:
                life = Player1Life + Player2Life;
                if (life % 2 != 0) {
                    life += 1;
                }
                life /= 2;
                Player1Life = life;
                Player2Life = life;
                setLifePoint(tvPlayer1Life, Player1Life);
                setLifePoint(tvPlayer2Life, Player2Life);
                break;

            case R.id.btnP2Divide:
                life = Player1Life + Player2Life;
                if (life % 2 != 0) {
                    life += 1;
                }
                life /= 2;
                Player1Life = life;
                Player2Life = life;
                setLifePoint(tvPlayer1Life, Player1Life);
                setLifePoint(tvPlayer2Life, Player2Life);
                break;

            case R.id.btnP1Opt:
                Player1Life = Player2Life;
                setLifePoint(tvPlayer1Life, Player1Life);
                break;

            case R.id.btnP2Opt:
                Player2Life = Player1Life;
                setLifePoint(tvPlayer2Life, Player2Life);
                break;

            case R.id.btnDice:
                Intent inDice = new Intent(getActivity(), CoinDiceActivity.class);
                inDice.putExtra("type", 1);
                startActivity(inDice);
                break;
            case R.id.btnCoin:
                Intent inCoin = new Intent(getActivity(), CoinDiceActivity.class);
                inCoin.putExtra("type", 2);
                startActivity(inCoin);
                break;
        }

    }

    private void setLifePoint(TextView tv, int life) {
        tv.setText(String.valueOf(life));
//        if (life >= 4000) {
//            tv.setTextColor(0xFF00FF00);
//        } else if (life < 1000) {
//            tv.setTextColor(0xFFFF0000);
//        } else {
//            tv.setTextColor(0xFFFF8C00);
//        }
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
