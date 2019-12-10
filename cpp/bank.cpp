#include <bits/stdc++.h>

using namespace std;

int current_id = 0;

struct conta {
    int balance;
    int id;
    mutex mt;

    conta( int initial_balance = 0 ) 
    {
        balance = initial_balance;
        id = current_id++;
        mt.unlock();
    }

    void print_acc()
    {
        cout << "conta id = " << id << " balance = " << balance << endl;
    }
    

    bool const operator<( const conta& rhs ) { return id < rhs.id; }
};

void deposit( conta& acc, int valor ) { acc.balance += valor; }
void withdraw( conta& acc, int valor ) { acc.balance -= valor; }

bool transfer(int value, conta& from, conta& to) {
    // temos que estipular uma ordem das locks, para nao ter deadlock com a transacao oposta
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
        // A thread ainda pode ser interrompida entre essas duas chamadas
        // gerando uma visao de mundo inconsistente para alguma outra thread...
        deposit(to, value);
        to.mt.unlock();
        from.mt.unlock();
        return true;
    }
    
    to.mt.unlock();
    from.mt.unlock();
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

int main()
{
    auto print_accs = [&] ( vector<conta>& v ) {
        for( auto& acc : v ) acc.print_acc();
    };

    vector< conta > vec(3);
    deposit(vec[0], 40);
    deposit(vec[1], 50);
    
    print_accs(vec);
    
    int v1 = 49;
    int v2 = 20;
    
    thread threads[2];
    threads[0] = thread(custom_transfer, v1, &vec[0], &vec[1], &vec[2]);
    threads[1] = thread(custom_transfer, v2, &vec[2], &vec[0], &vec[1]);
    
    print_accs(vec);

    for(auto& th : threads) th.join();

    return 0;
}



