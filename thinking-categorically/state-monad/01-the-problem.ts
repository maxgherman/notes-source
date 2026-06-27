// Manual state management approach
class GameState {
    health: number = 100;
    score: number = 0;
    inventory: string[] = [];
    position: { x: number, y: number } = { x: 0, y: 0 };
}

function collectItem(state: GameState, item: string): { result: string, newState: GameState } {
    const newState = { ...state };
    newState.inventory.push(item);
    newState.score += 10;
    return { result: `Collected ${item}`, newState };
}

function takeDamage(state: GameState, damage: number): { result: string, newState: GameState } {
    const newState = { ...state };
    newState.health = Math.max(0, newState.health - damage);
    return { result: newState.health > 0 ? "Still alive" : "Game over", newState };
}

function movePlayer(state: GameState, dx: number, dy: number): { result: string, newState: GameState } {
    const newState = { ...state };
    newState.position.x += dx;
    newState.position.y += dy;
    return { result: `Moved to (${newState.position.x}, ${newState.position.y})`, newState };
}

// Manual state threading
function playGame(): GameState {
    let state = new GameState();

    let result1 = collectItem(state, "sword");
    state = result1.newState;
    console.log(result1.result);

    let result2 = takeDamage(state, 20);
    state = result2.newState;
    console.log(result2.result);

    let result3 = movePlayer(state, 5, 3);
    state = result3.newState;
    console.log(result3.result);

    return state;
}

playGame();