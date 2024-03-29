#include <bits/stdc++.h>

using namespace std;

int current_id = 0;
atomic<int> ongoing_transfers(0);
atomic<int> checking_balances(0);
mutex balance_m;


struct conta {
    int balance;
    int id;
    mutex mt;

    conta(int inital_balance = 0) 
    {
        balance = 0;
        id = current_id++;
        mt.unlock();
    }

    void print_acc()
    {
        cout << "conta id = " << id << " balance = " << balance << endl;
    }

    bool operator<( const conta& rhs ) { return id < rhs.id; }
};

void deposit( conta& acc, int valor ) { acc.balance += valor; }
void withdraw( conta& acc, int valor ) { acc.balance -= valor; }

bool transfer(int value, conta& from, conta& to) {
    // enquanto tiver alguma thread checando os saldos, nao podemos realizar transferencias 
    while( checking_balances > 0 ) this_thread::sleep_for(chrono::milliseconds(5)); 
    
    ongoing_transfers++;

    if( from < to )
    {
        from.mt.lock();
        to.mt.lock();
    }
    else
    {
        to.mt.lock();
        from.mt.lock();
    }
    
    if( value < from.balance ) 
    {
        withdraw(from, value);
        
        deposit(to, value);
        to.mt.unlock();
        from.mt.unlock();
        ongoing_transfers--; 
        balance_m.unlock(); 
        return true;
    }
    
    to.mt.unlock();
    from.mt.unlock();
    ongoing_transfers--; 
    balance_m.unlock(); 
    return false;
}

// Essa funcao é fortemente inspirada no orElse de STM
void custom_transfer(int value, conta& from1, conta& from2, conta& to)
{
    int x = 0;
    bool ok = false;
    while(!ok)
    {
        if( x % 2 == 0 ) ok |= transfer(value, from1, to);
        else ok |= transfer(value, from2, to);
        ++x;
    }
}

// Para que essa funcao nao tenha uma visao inconsistente de mundo em nenhum momento,
// Precisamos pegar dar lock no mutex, a fim de garantir que nao tem nenhuma transferencia acontecendo 
bool check_global_balance(const vector<conta>& contas, int total_expected_balance ) 
{
   
    while( ongoing_transfers > 0 ) this_thread::sleep_for(chrono::milliseconds(5)); 
    checking_balances++;

    int total_bal = accumulate(contas.begin(), contas.end(), 0, 
    [&](const int& i, const conta& c) 
    { 
        return i + c.balance; 
    });
    
    checking_balances--;
    return (total_bal == total_expected_balance);
}


int main()
{
    auto print_accs = [&] ( vector<conta>& v ) {
        for( auto& acc : v ) acc.print_acc();
    };
    
    balance_m.unlock();
    
    vector< conta > vec(3);
    deposit(vec[0], 40);
    deposit(vec[1], 50);
    
    print_accs(vec);
    
    int v1 = 49;
    int v2 = 20;
   
    thread fst( [&] { custom_transfer( v1, vec[0], vec[1], vec[2] );} );
    thread snd( [&] { custom_transfer( v2, vec[2], vec[0], vec[1] );} );
     
    fst.join(); snd.join();
    
    print_accs(vec);

    return 0;
}
